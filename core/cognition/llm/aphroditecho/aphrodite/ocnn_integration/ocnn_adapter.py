"""
OCNN Adapter for Aphrodite Engine
Integrates OCNN neural network components with Aphrodite's cognitive architecture
"""

from typing import Dict, List, Any, Optional, Tuple
import numpy as np
import torch
import torch.nn as nn
from pathlib import Path


class OCNNPatternEncoder(nn.Module):
    """
    Spatial pattern encoder using OCNN-inspired convolution
    Encodes hypergraph patterns into neural representations
    """
    
    def __init__(self, input_dim: int = 512, hidden_dim: int = 256, output_dim: int = 128):
        super().__init__()
        self.input_dim = input_dim
        self.hidden_dim = hidden_dim
        self.output_dim = output_dim
        
        # Spatial convolution layers (inspired by OCNN)
        self.spatial_conv1 = nn.Conv1d(input_dim, hidden_dim, kernel_size=3, padding=1)
        self.spatial_conv2 = nn.Conv1d(hidden_dim, output_dim, kernel_size=3, padding=1)
        
        # Activation and normalization
        self.relu = nn.ReLU()
        self.batch_norm1 = nn.BatchNorm1d(hidden_dim)
        self.batch_norm2 = nn.BatchNorm1d(output_dim)
        
    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """
        Forward pass for pattern encoding
        
        Args:
            x: Input tensor of shape (batch, input_dim, sequence_length)
            
        Returns:
            Encoded pattern tensor of shape (batch, output_dim, sequence_length)
        """
        # First spatial convolution
        x = self.spatial_conv1(x)
        x = self.batch_norm1(x)
        x = self.relu(x)
        
        # Second spatial convolution
        x = self.spatial_conv2(x)
        x = self.batch_norm2(x)
        x = self.relu(x)
        
        return x


class OCNNTemporalProcessor(nn.Module):
    """
    Temporal sequence processor using OCNN-inspired temporal convolution
    Processes episodic memory sequences
    """
    
    def __init__(self, input_dim: int = 128, hidden_dim: int = 256, output_dim: int = 128):
        super().__init__()
        self.input_dim = input_dim
        self.hidden_dim = hidden_dim
        self.output_dim = output_dim
        
        # Temporal convolution layers
        self.temporal_conv1 = nn.Conv1d(input_dim, hidden_dim, kernel_size=5, padding=2)
        self.temporal_conv2 = nn.Conv1d(hidden_dim, output_dim, kernel_size=5, padding=2)
        
        # LSTM for sequence modeling
        self.lstm = nn.LSTM(output_dim, output_dim, batch_first=True)
        
        # Activation and normalization
        self.relu = nn.ReLU()
        self.batch_norm1 = nn.BatchNorm1d(hidden_dim)
        self.batch_norm2 = nn.BatchNorm1d(output_dim)
        
    def forward(self, x: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Forward pass for temporal processing
        
        Args:
            x: Input tensor of shape (batch, input_dim, sequence_length)
            
        Returns:
            Tuple of (processed_sequence, final_hidden_state)
        """
        # Temporal convolutions
        x = self.temporal_conv1(x)
        x = self.batch_norm1(x)
        x = self.relu(x)
        
        x = self.temporal_conv2(x)
        x = self.batch_norm2(x)
        x = self.relu(x)
        
        # LSTM processing
        x = x.transpose(1, 2)  # (batch, sequence_length, output_dim)
        lstm_out, (hidden, cell) = self.lstm(x)
        
        return lstm_out, hidden


class OCNNAttentionBridge(nn.Module):
    """
    Attention mechanism bridge between OCNN and Aphrodite
    Implements cross-modal attention for pattern integration
    """
    
    def __init__(self, dim: int = 128, num_heads: int = 8):
        super().__init__()
        self.dim = dim
        self.num_heads = num_heads
        
        # Multi-head attention
        self.attention = nn.MultiheadAttention(dim, num_heads, batch_first=True)
        
        # Layer normalization
        self.layer_norm = nn.LayerNorm(dim)
        
        # Feed-forward network
        self.ffn = nn.Sequential(
            nn.Linear(dim, dim * 4),
            nn.ReLU(),
            nn.Linear(dim * 4, dim)
        )
        
    def forward(self, query: torch.Tensor, key: torch.Tensor, value: torch.Tensor) -> torch.Tensor:
        """
        Forward pass for attention bridge
        
        Args:
            query: Query tensor
            key: Key tensor
            value: Value tensor
            
        Returns:
            Attention output tensor
        """
        # Multi-head attention
        attn_out, attn_weights = self.attention(query, key, value)
        
        # Residual connection and layer norm
        x = self.layer_norm(query + attn_out)
        
        # Feed-forward network with residual
        ffn_out = self.ffn(x)
        x = self.layer_norm(x + ffn_out)
        
        return x


class OCNNAdapter:
    """
    Main adapter class for integrating OCNN with Aphrodite Engine
    """
    
    def __init__(self, device: str = "cpu"):
        self.device = torch.device(device)
        
        # Initialize OCNN components
        self.pattern_encoder = OCNNPatternEncoder().to(self.device)
        self.temporal_processor = OCNNTemporalProcessor().to(self.device)
        self.attention_bridge = OCNNAttentionBridge().to(self.device)
        
        # Set to evaluation mode by default
        self.pattern_encoder.eval()
        self.temporal_processor.eval()
        self.attention_bridge.eval()
        
    def encode_hypergraph_pattern(self, pattern_data: np.ndarray) -> np.ndarray:
        """
        Encode hypergraph pattern using OCNN spatial convolution
        
        Args:
            pattern_data: Numpy array of pattern data
            
        Returns:
            Encoded pattern as numpy array
        """
        with torch.no_grad():
            # Convert to tensor
            x = torch.from_numpy(pattern_data).float().to(self.device)
            
            # Ensure correct shape (batch, channels, sequence)
            if x.dim() == 2:
                x = x.unsqueeze(0)
            
            # Encode
            encoded = self.pattern_encoder(x)
            
            # Convert back to numpy
            return encoded.cpu().numpy()
    
    def process_episodic_sequence(self, sequence_data: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """
        Process episodic memory sequence using OCNN temporal convolution
        
        Args:
            sequence_data: Numpy array of sequence data
            
        Returns:
            Tuple of (processed_sequence, final_state)
        """
        with torch.no_grad():
            # Convert to tensor
            x = torch.from_numpy(sequence_data).float().to(self.device)
            
            # Ensure correct shape
            if x.dim() == 2:
                x = x.unsqueeze(0)
            
            # Process
            sequence, hidden = self.temporal_processor(x)
            
            # Convert back to numpy
            return sequence.cpu().numpy(), hidden.cpu().numpy()
    
    def apply_cross_modal_attention(
        self,
        query_data: np.ndarray,
        key_data: np.ndarray,
        value_data: np.ndarray
    ) -> np.ndarray:
        """
        Apply cross-modal attention between different cognitive components
        
        Args:
            query_data: Query numpy array
            key_data: Key numpy array
            value_data: Value numpy array
            
        Returns:
            Attention output as numpy array
        """
        with torch.no_grad():
            # Convert to tensors
            query = torch.from_numpy(query_data).float().to(self.device)
            key = torch.from_numpy(key_data).float().to(self.device)
            value = torch.from_numpy(value_data).float().to(self.device)
            
            # Ensure correct shapes
            if query.dim() == 2:
                query = query.unsqueeze(0)
            if key.dim() == 2:
                key = key.unsqueeze(0)
            if value.dim() == 2:
                value = value.unsqueeze(0)
            
            # Apply attention
            output = self.attention_bridge(query, key, value)
            
            # Convert back to numpy
            return output.cpu().numpy()
    
    def extract_activation_trace(self, input_data: np.ndarray) -> Dict[str, np.ndarray]:
        """
        Extract activation traces from OCNN processing
        Used for hypergraph edge annotations
        
        Args:
            input_data: Input numpy array
            
        Returns:
            Dictionary of activation traces
        """
        with torch.no_grad():
            x = torch.from_numpy(input_data).float().to(self.device)
            
            if x.dim() == 2:
                x = x.unsqueeze(0)
            
            # Get activations from each layer
            spatial_act = self.pattern_encoder(x)
            temporal_seq, temporal_hidden = self.temporal_processor(spatial_act)
            
            return {
                "spatial_activation": spatial_act.cpu().numpy(),
                "temporal_sequence": temporal_seq.cpu().numpy(),
                "temporal_hidden": temporal_hidden.cpu().numpy()
            }
    
    def save_models(self, save_dir: Path):
        """Save OCNN models"""
        save_dir.mkdir(parents=True, exist_ok=True)
        
        torch.save(self.pattern_encoder.state_dict(), save_dir / "pattern_encoder.pt")
        torch.save(self.temporal_processor.state_dict(), save_dir / "temporal_processor.pt")
        torch.save(self.attention_bridge.state_dict(), save_dir / "attention_bridge.pt")
        
        print(f"✅ OCNN models saved to {save_dir}")
    
    def load_models(self, load_dir: Path):
        """Load OCNN models"""
        self.pattern_encoder.load_state_dict(torch.load(load_dir / "pattern_encoder.pt"))
        self.temporal_processor.load_state_dict(torch.load(load_dir / "temporal_processor.pt"))
        self.attention_bridge.load_state_dict(torch.load(load_dir / "attention_bridge.pt"))
        
        print(f"✅ OCNN models loaded from {load_dir}")


def test_ocnn_adapter():
    """Test OCNN adapter functionality"""
    print("🧪 Testing OCNN Adapter...")
    
    # Initialize adapter
    adapter = OCNNAdapter(device="cpu")
    
    # Test pattern encoding
    pattern_data = np.random.randn(512, 10)
    encoded = adapter.encode_hypergraph_pattern(pattern_data)
    print(f"✅ Pattern encoding: {pattern_data.shape} -> {encoded.shape}")
    
    # Test temporal processing
    sequence_data = np.random.randn(128, 20)
    sequence, hidden = adapter.process_episodic_sequence(sequence_data)
    print(f"✅ Temporal processing: {sequence_data.shape} -> {sequence.shape}, {hidden.shape}")
    
    # Test attention
    query = np.random.randn(10, 128)
    key = np.random.randn(10, 128)
    value = np.random.randn(10, 128)
    attn_out = adapter.apply_cross_modal_attention(query, key, value)
    print(f"✅ Cross-modal attention: {query.shape} -> {attn_out.shape}")
    
    # Test activation trace
    input_data = np.random.randn(512, 10)
    traces = adapter.extract_activation_trace(input_data)
    print(f"✅ Activation trace extracted: {len(traces)} traces")
    
    print("✨ OCNN Adapter tests passed!")


if __name__ == "__main__":
    test_ocnn_adapter()
