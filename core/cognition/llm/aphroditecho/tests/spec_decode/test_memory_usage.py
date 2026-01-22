import torch
import aphrodite
from tests.core.utils import create_dummy_prompt
from aphrodite.common.sequence import SequenceGroup
ITERATIONS = 100
MAIN_MODEL = 'JackFram/llama-68m'
SPEC_MODEL = 'abhigoyal/aphrodite-medusa-llama-68m-random'
BATCH_SIZE = 5
SPEC_DISABLE_BATCH_SIZE = 2
def add_seq_group_to_engine(engine: aphrodite.AphroditeEngine, seq_group: SequenceGroup):
    scheduler = engine.scheduler[0]
    scheduler.add_seq_group(seq_group)
'\nSince we are using a batch size greater than the disabled batch size, \nwe can ensure we go through the _no_spec codepath for most of our engine steps.\n'
def test_memory_usage_no_spec():
    previous_memory_allocated = None
    llm = aphrodite.LLM(model=MAIN_MODEL, speculative_model=SPEC_MODEL, num_speculative_tokens=3, speculative_disable_by_batch_size=SPEC_DISABLE_BATCH_SIZE)
    batch_sequences = set()
    engine = llm.llm_engine
    for i in range(ITERATIONS):
        seq, seq_group = create_dummy_prompt(request_id=str(i), prompt_length=10, min_tokens=10, max_tokens=10)
        add_seq_group_to_engine(engine, seq_group)
        batch_sequences.add(seq)
        engine.step()
        for seq in list(batch_sequences):
            if seq.is_finished():
                batch_sequences.remove(seq)
        if len(batch_sequences) <= BATCH_SIZE:
            continue
        while not any((seq.is_finished() for seq in batch_sequences)):
            engine.step()
        for seq in list(batch_sequences):
            if seq.is_finished():
                batch_sequences.remove(seq)
        if previous_memory_allocated is None:
            previous_memory_allocated = torch.cuda.memory_allocated()
        else:
            assert previous_memory_allocated == torch.cuda.memory_allocated()