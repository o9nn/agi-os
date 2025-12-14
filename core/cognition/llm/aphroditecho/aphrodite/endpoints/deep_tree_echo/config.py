"""
Configuration module for Deep Tree Echo endpoints.

Provides configuration management for DTESN server-side rendering endpoints.
"""

import os
from typing import List
from pydantic import BaseModel, Field


class DTESNConfig(BaseModel):
    """Configuration for Deep Tree Echo endpoints."""
    
    # Server configuration
    enable_docs: bool = Field(
        default=True,
        description="Enable OpenAPI documentation endpoints"
    )
    
    allowed_origins: List[str] = Field(
        default=["*"],
        description="CORS allowed origins for server-side requests"
    )
    
    # DTESN processing configuration
    max_membrane_depth: int = Field(
        default=8,
        description="Maximum depth for DTESN membrane hierarchy"
    )
    
    esn_reservoir_size: int = Field(
        default=1024,
        description="Size of Echo State Network reservoir"
    )
    
    bseries_max_order: int = Field(
        default=16,
        description="Maximum order for B-Series computations"
    )
    
    # Performance configuration
    enable_caching: bool = Field(
        default=True,
        description="Enable server-side response caching"
    )
    
    cache_ttl_seconds: int = Field(
        default=300,
        description="Cache TTL in seconds"
    )
    
    enable_performance_monitoring: bool = Field(
        default=True,
        description="Enable performance monitoring middleware"
    )
    
    @classmethod
    def from_env(cls) -> "DTESNConfig":
        """Create configuration from environment variables."""
        return cls(
            enable_docs=os.getenv("DTESN_ENABLE_DOCS", "true").lower() == "true",
            max_membrane_depth=int(os.getenv("DTESN_MAX_MEMBRANE_DEPTH", "8")),
            esn_reservoir_size=int(os.getenv("DTESN_ESN_RESERVOIR_SIZE", "1024")),
            bseries_max_order=int(os.getenv("DTESN_BSERIES_MAX_ORDER", "16")),
            enable_caching=os.getenv("DTESN_ENABLE_CACHING", "true").lower() == "true",
            cache_ttl_seconds=int(os.getenv("DTESN_CACHE_TTL_SECONDS", "300")),
            enable_performance_monitoring=os.getenv("DTESN_ENABLE_PERFORMANCE_MONITORING", "true").lower() == "true",
        )