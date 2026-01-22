import ast
from dataclasses import replace
from typing import Optional
import numpy as np
import torch
import torch.nn as nn
from loguru import logger
from aphrodite.attention.layer import Attention
from aphrodite.common.config import AphroditeConfig, CompilationLevel, get_layers_from_aphrodite_config
from aphrodite.distributed.parallel_state import get_pp_group
from aphrodite.forward_context import set_forward_context
from aphrodite.modeling.model_loader import get_model
from aphrodite.modeling.models import supports_multimodal
from aphrodite.modeling.models.llama_eagle3 import Eagle3LlamaForCausalLM
from aphrodite.utils import is_pin_memory_available
from aphrodite.v1.attention.backends.flash_attn import FlashAttentionMetadata
from aphrodite.v1.attention.backends.tree_attn import TreeAttentionMetadata, TreeAttentionMetadataBuilder
from aphrodite.v1.attention.backends.utils import CommonAttentionMetadata
from aphrodite.v1.kv_cache_interface import KVCacheConfig
from aphrodite.v1.sample.metadata import SamplingMetadata
PADDING_SLOT_ID = -1
class EagleProposer:
    def __init__(self, aphrodite_config: AphroditeConfig, device: torch.device, runner=None):
        self.aphrodite_config = aphrodite_config
        self.speculative_config = aphrodite_config.speculative_config
        self.draft_model_config = self.speculative_config.draft_model_config
        self.method = self.speculative_config.method
        self.runner = runner
        self.dtype = aphrodite_config.model_config.dtype
        self.max_model_len = aphrodite_config.model_config.max_model_len
        self.block_size = aphrodite_config.cache_config.block_size
        self.num_speculative_tokens = self.speculative_config.num_speculative_tokens
        self.max_num_tokens = aphrodite_config.scheduler_config.max_num_batched_tokens
        self.token_arange_np = np.arange(self.max_num_tokens)
        self.hidden_size = self.draft_model_config.get_hidden_size()
        self.is_multimodal_model = aphrodite_config.model_config.is_multimodal_model
        self.use_cuda_graph = self.aphrodite_config.compilation_config.level == CompilationLevel.PIECEWISE and (not self.aphrodite_config.model_config.enforce_eager)
        self.cudagraph_batch_sizes = list(reversed(self.aphrodite_config.compilation_config.cudagraph_capture_sizes))
        self.input_ids = torch.zeros(self.max_num_tokens, dtype=torch.int32, device=device)
        self.positions = torch.zeros(self.max_num_tokens, dtype=torch.int64, device=device)
        self.hidden_states = torch.zeros((self.max_num_tokens, self.hidden_size), dtype=self.dtype, device=device)
        max_batch_size = aphrodite_config.scheduler_config.max_num_seqs
        self.arange = torch.arange(max_batch_size + 1, device=device, dtype=torch.int32)
        self.inputs_embeds = torch.zeros((self.max_num_tokens, self.hidden_size), dtype=self.dtype, device=device)
        spec_token_tree = self.speculative_config.speculative_token_tree
        self.tree_choices: list[tuple[int, ...]] = ast.literal_eval(spec_token_tree)
        tree_depth = len(self.tree_choices[-1])
        num_drafts_per_level = [0] * tree_depth
        for node in self.tree_choices:
            num_drafts_per_level[len(node) - 1] += 1
        self.cu_drafts_per_level = [num_drafts_per_level[0]]
        self.child_drafts_per_level = [num_drafts_per_level[0]]
        for level in range(1, tree_depth):
            self.cu_drafts_per_level.append(self.cu_drafts_per_level[-1] + num_drafts_per_level[level])
            self.child_drafts_per_level.append(num_drafts_per_level[level] // num_drafts_per_level[level - 1])
        self.first_branching_level = None
        for level in range(tree_depth):
            if self.cu_drafts_per_level[level] > level + 1:
                self.first_branching_level = level
                break
        self.tree_draft_pos_offsets = torch.arange(1, len(self.tree_choices) + 1, device=device, dtype=torch.int32).repeat(max_batch_size, 1)
    def propose(self, target_token_ids: torch.Tensor, target_positions: torch.Tensor, target_hidden_states: torch.Tensor, next_token_ids: torch.Tensor, common_attn_metadata: CommonAttentionMetadata, sampling_metadata: SamplingMetadata, mm_embeds: Optional[list[torch.Tensor]]=None) -> torch.Tensor:
        num_tokens = target_token_ids.shape[0]
        batch_size = next_token_ids.shape[0]
        last_token_indices = common_attn_metadata.query_start_loc[1:] - 1
        if self.method == 'eagle3':
            assert isinstance(self.model, Eagle3LlamaForCausalLM)
            target_hidden_states = self.model.combine_hidden_states(target_hidden_states)
            assert target_hidden_states.shape[-1] == self.hidden_size
        self.input_ids[:num_tokens - 1] = target_token_ids[1:]
        self.input_ids[last_token_indices] = next_token_ids
        assert self.runner is not None
        attn_metadata = self.runner.attn_metadata_builders[0].build_for_drafting(common_attn_metadata=common_attn_metadata, draft_index=0)
        per_layer_attn_metadata = {}
        for layer_name in self.attn_layer_names:
            per_layer_attn_metadata[layer_name] = attn_metadata
        if self.use_cuda_graph and num_tokens <= self.cudagraph_batch_sizes[-1]:
            num_input_tokens = self.aphrodite_config.pad_for_cudagraph(num_tokens)
        else:
            num_input_tokens = num_tokens
        self.positions[:num_tokens] = target_positions
        self.hidden_states[:num_tokens] = target_hidden_states
        if self.is_multimodal_model:
            input_ids = self.input_ids[:num_tokens]
            inputs_embeds = self.model.get_input_embeddings(input_ids, multimodal_embeddings=mm_embeds or None)
            self.inputs_embeds[:num_tokens] = inputs_embeds
            inputs_embeds = self.inputs_embeds[:num_input_tokens]
            input_ids = None
        else:
            inputs_embeds = None
            input_ids = self.input_ids[:num_input_tokens]
        with set_forward_context(per_layer_attn_metadata, self.aphrodite_config, num_tokens=num_input_tokens):
            ret_hidden_states = self.model(input_ids=input_ids, positions=self.positions[:num_input_tokens], hidden_states=self.hidden_states[:num_input_tokens], inputs_embeds=inputs_embeds)
            if self.method == 'deepseek_mtp':
                last_hidden_states = ret_hidden_states
            else:
                last_hidden_states, hidden_states = ret_hidden_states
        sample_hidden_states = last_hidden_states[last_token_indices]
        logits = self.model.compute_logits(sample_hidden_states, None)
        positions = target_positions[last_token_indices]
        hidden_states = hidden_states[last_token_indices]
        if self.first_branching_level == 0:
            draft_token_ids_list = self.propose_tree(tree_root_level=0, batch_size=batch_size, logits=logits, positions=positions, hidden_states=hidden_states, common_attn_metadata=common_attn_metadata)
            return torch.cat(draft_token_ids_list, dim=1)
        draft_token_ids = logits.argmax(dim=-1)
        if self.num_speculative_tokens == 1:
            return draft_token_ids.view(-1, 1)
        assert isinstance(attn_metadata, (FlashAttentionMetadata, TreeAttentionMetadata))
        draft_token_ids_list = [draft_token_ids]
        if self.use_cuda_graph and batch_size <= self.cudagraph_batch_sizes[-1]:
            input_batch_size = self.aphrodite_config.pad_for_cudagraph(batch_size)
        else:
            input_batch_size = batch_size
        attn_metadata.num_actual_tokens = batch_size
        attn_metadata.max_query_len = 1
        attn_metadata.query_start_loc = self.arange[:batch_size + 1]
        for token_index in range(self.num_speculative_tokens - 1):
            input_ids = draft_token_ids_list[-1].int()
            positions += 1
            exceeds_max_model_len = positions >= self.max_model_len
            clamped_positions = torch.where(exceeds_max_model_len, 0, positions)
            attn_metadata.max_seq_len += 1
            attn_metadata.seq_lens += 1
            attn_metadata.max_seq_len = min(attn_metadata.max_seq_len, self.max_model_len)
            attn_metadata.seq_lens.masked_fill_(exceeds_max_model_len, 1)
            block_numbers = clamped_positions // self.block_size
            block_ids = attn_metadata.block_table.gather(dim=1, index=block_numbers.view(-1, 1))
            block_ids = block_ids.view(-1)
            attn_metadata.slot_mapping = block_ids * self.block_size + clamped_positions % self.block_size
            attn_metadata.slot_mapping.masked_fill_(exceeds_max_model_len, PADDING_SLOT_ID)
            self.input_ids[:batch_size] = input_ids
            self.positions[:batch_size] = clamped_positions
            self.hidden_states[:batch_size] = hidden_states
            if self.is_multimodal_model:
                inputs_embeds = self.model.get_input_embeddings(input_ids)
                self.inputs_embeds[:batch_size] = inputs_embeds
                inputs_embeds = self.inputs_embeds[:input_batch_size]
                input_ids = None
            else:
                inputs_embeds = None
                input_ids = self.input_ids[:input_batch_size]
            with set_forward_context(per_layer_attn_metadata, self.aphrodite_config, num_tokens=input_batch_size):
                last_hidden_states, hidden_states = self.model(input_ids=input_ids, positions=self.positions[:input_batch_size], hidden_states=self.hidden_states[:input_batch_size], inputs_embeds=inputs_embeds)
            hidden_states = hidden_states[:batch_size]
            logits = self.model.compute_logits(last_hidden_states[:batch_size], None)
            if self.first_branching_level == token_index + 1:
                draft_token_ids_list += self.propose_tree(tree_root_level=token_index + 1, batch_size=batch_size, logits=logits, positions=positions, hidden_states=hidden_states, common_attn_metadata=common_attn_metadata)
                return torch.cat(draft_token_ids_list, dim=1)
            draft_token_ids = logits.argmax(dim=-1)
            draft_token_ids_list.append(draft_token_ids)
        draft_token_ids = torch.stack(draft_token_ids_list, dim=1)
        return draft_token_ids
    def propose_tree(self, tree_root_level: int, batch_size: int, logits: torch.Tensor, positions: torch.Tensor, hidden_states: torch.Tensor, common_attn_metadata: CommonAttentionMetadata) -> list[torch.Tensor]:
        tree_attn_metadata_builder = self.runner.attn_metadata_builders[0]
        assert isinstance(tree_attn_metadata_builder, TreeAttentionMetadataBuilder)
        total_num_drafts = self.cu_drafts_per_level[tree_root_level]
        level_num_drafts = total_num_drafts
        num_children = self.child_drafts_per_level[tree_root_level]
        if num_children == 1:
            draft_token_ids = logits.argmax(dim=-1).view(batch_size, -1)
        else:
            draft_token_ids = torch.topk(logits, num_children, dim=-1).indices.view(batch_size, -1)
        draft_token_ids_list = [draft_token_ids]
        draft_hidden_states = hidden_states.view(batch_size, 1, -1)
        tree_input_ids = torch.empty(0, device=self.input_ids.device, dtype=self.input_ids.dtype)
        tree_positions = torch.empty(0, device=self.positions.device, dtype=self.positions.dtype)
        tree_hidden_states = torch.empty(0, device=self.hidden_states.device, dtype=self.hidden_states.dtype)
        flattened_draft_positions = positions.view(batch_size, -1) + self.tree_draft_pos_offsets[:batch_size, :]
        tree_depth = len(self.cu_drafts_per_level)
        for level in range(tree_root_level, tree_depth - 1):
            draft_positions = positions + (level + 1)
            exceeds_max_model_len = positions + total_num_drafts >= self.max_model_len
            clamped_draft_positions = torch.where(exceeds_max_model_len, 0, draft_positions)
            if level_num_drafts > 1:
                draft_positions = clamped_draft_positions.repeat_interleave(level_num_drafts).reshape(batch_size, -1)
            if num_children > 1:
                draft_hidden_states = draft_hidden_states.repeat_interleave(num_children, dim=1)
            tree_input_ids = torch.cat([tree_input_ids, draft_token_ids], dim=1)
            tree_positions = torch.cat([tree_positions, draft_positions], dim=1)
            tree_hidden_states = torch.cat([tree_hidden_states, draft_hidden_states], dim=1)
            query_len = total_num_drafts - tree_root_level
            common_attn_metadata = replace(common_attn_metadata, query_start_loc=query_len * self.arange[:batch_size + 1], seq_lens=common_attn_metadata.seq_lens + level_num_drafts, num_actual_tokens=batch_size * query_len, max_query_len=query_len)
            attn_metadata = tree_attn_metadata_builder.build_for_drafting(common_attn_metadata=common_attn_metadata, draft_index=tree_root_level + 1)
            per_layer_attn_metadata = {}
            for layer_name in self.attn_layer_names:
                per_layer_attn_metadata[layer_name] = attn_metadata
            attn_metadata.max_seq_len = min(attn_metadata.max_seq_len, self.max_model_len)
            attn_metadata.seq_lens.masked_fill_(exceeds_max_model_len, 1)
            query_positions = flattened_draft_positions[:, level:level + query_len]
            block_numbers = query_positions // self.block_size
            block_ids = attn_metadata.block_table.gather(dim=1, index=block_numbers)
            slot_mapping = block_ids * self.block_size + query_positions % self.block_size
            slot_mapping[exceeds_max_model_len] = PADDING_SLOT_ID
            attn_metadata.slot_mapping = slot_mapping.view(-1)
            num_tokens = attn_metadata.num_actual_tokens
            input_ids = tree_input_ids.view(-1)
            self.input_ids[:num_tokens] = input_ids
            self.positions[:num_tokens] = tree_positions.view(-1)
            self.hidden_states[:num_tokens] = tree_hidden_states.view(num_tokens, -1)
            if self.use_cuda_graph and num_tokens <= self.cudagraph_batch_sizes[-1]:
                num_input_tokens = self.aphrodite_config.pad_for_cudagraph(num_tokens)
            else:
                num_input_tokens = num_tokens
            with set_forward_context(per_layer_attn_metadata, self.aphrodite_config, num_tokens=num_input_tokens):
                last_hidden_states, hidden_states = self.model(input_ids=self.input_ids[:num_input_tokens], positions=self.positions[:num_input_tokens], hidden_states=self.hidden_states[:num_input_tokens], inputs_embeds=None)
            draft_hidden_states = hidden_states[:num_tokens].view(batch_size, query_len, -1)[:, -level_num_drafts:]
            draft_last_hidden_states = last_hidden_states[:num_tokens].view(batch_size, query_len, -1)[:, -level_num_drafts:]
            logits = self.model.compute_logits(draft_last_hidden_states.reshape(batch_size * level_num_drafts, -1), None)
            num_children = self.child_drafts_per_level[level + 1]
            if num_children == 1:
                draft_token_ids = logits.argmax(dim=-1).view(batch_size, -1)
            else:
                draft_token_ids = torch.topk(logits, num_children, dim=-1).indices.view(batch_size, -1)
            draft_token_ids_list.append(draft_token_ids)
            level_num_drafts = self.cu_drafts_per_level[level + 1] - total_num_drafts
            total_num_drafts = self.cu_drafts_per_level[level + 1]
        return draft_token_ids_list
    def prepare_inputs(self, common_attn_metadata: CommonAttentionMetadata, num_rejected_tokens: torch.Tensor) -> tuple[CommonAttentionMetadata, torch.Tensor]:
        device = common_attn_metadata.query_start_loc.device
        query_start_loc_cpu = common_attn_metadata.query_start_loc_cpu
        new_seq_lens_cpu = common_attn_metadata.seq_lens_cpu - num_rejected_tokens
        new_query_len_per_req = query_start_loc_cpu[1:] - query_start_loc_cpu[:-1]
        new_num_tokens_per_req = new_query_len_per_req - num_rejected_tokens
        new_num_tokens_per_req_np = new_num_tokens_per_req.numpy()
        new_query_start_loc_cpu = torch.zeros(query_start_loc_cpu.shape, dtype=torch.int32, pin_memory=is_pin_memory_available())
        new_query_start_loc_np = new_query_start_loc_cpu.numpy()
        np.cumsum(new_num_tokens_per_req_np, out=new_query_start_loc_np[1:])
        total_num_tokens = new_query_start_loc_np[-1]
        new_query_start_locs_expanded = np.repeat(new_query_start_loc_np[:-1], new_num_tokens_per_req_np)
        token_offests = self.token_arange_np[:total_num_tokens] - new_query_start_locs_expanded
        old_query_start_locs_expanded = np.repeat(query_start_loc_cpu[:-1].numpy(), new_num_tokens_per_req_np)
        token_indices_np = token_offests + old_query_start_locs_expanded
        token_indices = torch.from_numpy(token_indices_np).to(device, non_blocking=True)
        spec_common_attn_metadata = CommonAttentionMetadata(query_start_loc=new_query_start_loc_cpu.to(device, non_blocking=True), seq_lens=new_seq_lens_cpu.to(device, non_blocking=True), query_start_loc_cpu=new_query_start_loc_cpu, seq_lens_cpu=new_seq_lens_cpu, num_computed_tokens_cpu=common_attn_metadata.num_computed_tokens_cpu, num_reqs=common_attn_metadata.num_reqs, num_actual_tokens=total_num_tokens, max_query_len=new_query_len_per_req.max().item(), block_table_tensor=common_attn_metadata.block_table_tensor, slot_mapping=common_attn_metadata.slot_mapping[token_indices], causal=True)
        return (spec_common_attn_metadata, token_indices)
    def load_model(self, target_model: nn.Module) -> None:
        draft_model_config = self.aphrodite_config.speculative_config.draft_model_config
        target_attn_layer_names = set(get_layers_from_aphrodite_config(self.aphrodite_config, Attention).keys())
        from aphrodite.compilation.backends import set_model_tag
        with set_model_tag('eagle_head'):
            self.model = get_model(aphrodite_config=self.aphrodite_config, model_config=draft_model_config)
        draft_attn_layer_names = get_layers_from_aphrodite_config(self.aphrodite_config, Attention).keys() - target_attn_layer_names
        self.attn_layer_names = list(draft_attn_layer_names)
        if supports_multimodal(target_model):
            self.model.config.image_token_index = target_model.config.image_token_index
            target_language_model = target_model.get_language_model()
        else:
            target_language_model = target_model
        if get_pp_group().world_size == 1 and self.model.model.embed_tokens.weight.shape == target_language_model.model.embed_tokens.weight.shape:
            logger.info('Assuming the EAGLE head shares the same vocab embedding with the target model.')
            del self.model.model.embed_tokens
            self.model.model.embed_tokens = target_language_model.model.embed_tokens
        else:
            logger.info("The EAGLE head's vocab embedding will be loaded separately from the target model.")
        if self.aphrodite_config.speculative_config.method != 'eagle3' and hasattr(target_language_model, 'lm_head'):
            logger.info('Loading EAGLE LM head weights from the target model.')
            self.model.lm_head = target_language_model.lm_head
    @torch.inference_mode()
    def dummy_run(self, num_tokens: int) -> None:
        with set_forward_context(None, self.aphrodite_config, num_tokens=num_tokens):
            if self.is_multimodal_model:
                input_ids = None
                inputs_embeds = self.inputs_embeds[:num_tokens]
            else:
                input_ids = self.input_ids[:num_tokens]
                inputs_embeds = None
            self.model(input_ids=input_ids, positions=self.positions[:num_tokens], hidden_states=self.hidden_states[:num_tokens], inputs_embeds=inputs_embeds)
    def validate_same_kv_cache_group(self, kv_cache_config: KVCacheConfig) -> None:
        kv_cache_groups: dict[str, int] = {}
        for id, kv_cache_group in enumerate(kv_cache_config.kv_cache_groups):
            for layer_name in kv_cache_group.layer_names:
                kv_cache_groups[layer_name] = id
        assert len(set([kv_cache_groups[layer_name] for layer_name in self.attn_layer_names])) == 1, 'All eagle layers should belong to the same kv cache group'
def compute_probs_and_sample_next_token(logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> tuple[torch.Tensor, torch.Tensor]:
    if sampling_metadata.all_greedy:
        probs = logits
        next_token_ids = logits.argmax(dim=-1)
        return (next_token_ids, probs)
    is_greedy = sampling_metadata.temperature == -1
    temperature = torch.where(is_greedy, 1.0, sampling_metadata.temperature)
    logits.div_(temperature.view(-1, 1))
    probs = logits.softmax(dim=-1, dtype=torch.float32)
    q = torch.empty_like(probs)
    q.exponential_()
    next_token_ids = probs.div(q).argmax(dim=-1).view(-1)
    if not sampling_metadata.all_random:
        greedy_token_ids = probs.argmax(dim=-1)
        next_token_ids = torch.where(is_greedy, greedy_token_ids, next_token_ids)
    return (next_token_ids, probs)