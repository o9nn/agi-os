"""
Enterprise security system for Aphrodite Engine.

Provides comprehensive audit logging, privacy compliance, security incident
response, and integration with Echo systems for enhanced AI security.

This module implements both legacy security middleware and new enterprise
security features including GDPR compliance, real-time threat detection,
and comprehensive audit trails.
"""

# Legacy Security Components
from .input_validation import (
    InputValidationMiddleware, 
    validate_request_input,
    validate_dtesn_endpoint_data
)
from .output_sanitization import (
    OutputSanitizationMiddleware,
    ErrorSanitizationMiddleware, 
    sanitize_response_output
)
from .security_middleware import (
    SecurityMiddleware, 
    RateLimitMiddleware
)
from .dtesn_validation import (
    DTESNDataType,
    DTESNValidationConfig,
    validate_dtesn_data_structure,
    normalize_dtesn_configuration
)
from .data_sanitization import (
    SanitizationLevel,
    DataFormat,
    SanitizationConfig,
    sanitize_data_value,
    create_sanitization_pipeline,
    dtesn_sanitizer,
    json_sanitizer
)

# Enterprise Security Components
from .audit_logger import (
    EnterpriseAuditLogger,
    AuditConfig,
    AuditEventType,
    AuditSeverity,
    get_audit_logger,
    configure_audit_logging,
    audit_log
)
from .privacy_compliance import (
    PrivacyComplianceManager,
    ConsentManager,
    DataRetentionManager,
    DataProcessingPurpose,
    DataCategory,
    PrivacyRegulation,
    ConsentStatus,
    get_privacy_manager,
    configure_privacy_compliance
)
from .incident_response import (
    IncidentResponseEngine,
    SecurityEvent,
    SecurityIncident,
    ThreatType,
    IncidentSeverity,
    IncidentStatus,
    ResponseAction,
    get_incident_engine,
    process_security_event
)
from .api_endpoints import (
    security_router,
    include_security_routes
)

# Enterprise Middleware Integration
from ..middleware.enterprise_audit_middleware import (
    EnterpriseAuditMiddleware,
    EnterpriseAuditConfig
)

__all__ = [
    # Legacy Security Components
    "InputValidationMiddleware",
    "OutputSanitizationMiddleware",
    "ErrorSanitizationMiddleware", 
    "SecurityMiddleware",
    "RateLimitMiddleware",
    "validate_request_input",
    "validate_dtesn_endpoint_data",
    "sanitize_response_output",
    "DTESNDataType",
    "DTESNValidationConfig", 
    "validate_dtesn_data_structure",
    "normalize_dtesn_configuration",
    "SanitizationLevel",
    "DataFormat",
    "SanitizationConfig",
    "sanitize_data_value",
    "create_sanitization_pipeline",
    "dtesn_sanitizer",
    "json_sanitizer",
    
    # Enterprise Audit Logging
    "EnterpriseAuditLogger",
    "AuditConfig",
    "AuditEventType",
    "AuditSeverity", 
    "get_audit_logger",
    "configure_audit_logging",
    "audit_log",
    
    # Privacy Compliance
    "PrivacyComplianceManager",
    "ConsentManager",
    "DataRetentionManager",
    "DataProcessingPurpose",
    "DataCategory",
    "PrivacyRegulation",
    "ConsentStatus",
    "get_privacy_manager",
    "configure_privacy_compliance",
    
    # Incident Response
    "IncidentResponseEngine",
    "SecurityEvent",
    "SecurityIncident",
    "ThreatType",
    "IncidentSeverity",
    "IncidentStatus",
    "ResponseAction",
    "get_incident_engine",
    "process_security_event",
    
    # API Endpoints
    "security_router",
    "include_security_routes",
    
    # Enterprise Middleware
    "EnterpriseAuditMiddleware",
    "EnterpriseAuditConfig"
]