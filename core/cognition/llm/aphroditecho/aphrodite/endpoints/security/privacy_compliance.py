"""
Data privacy and compliance reporting system for Aphrodite Engine.

Implements GDPR, CCPA, and other privacy regulations with comprehensive
data handling policies, consent management, and audit trails.
"""

from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional
from enum import Enum
from dataclasses import dataclass
import uuid

from fastapi import Request
import structlog

from .audit_logger import (
    AuditEventType, 
    AuditSeverity, 
    get_audit_logger
)

# Configure privacy logger
privacy_logger = structlog.get_logger("privacy")


class PrivacyRegulation(str, Enum):
    """Supported privacy regulations."""
    
    GDPR = "gdpr"  # General Data Protection Regulation (EU)
    CCPA = "ccpa"  # California Consumer Privacy Act (US)
    PIPEDA = "pipeda"  # Personal Information Protection and Electronic Documents Act (Canada)
    LGPD = "lgpd"  # Lei Geral de Proteção de Dados (Brazil)
    PDPA = "pdpa"  # Personal Data Protection Act (Singapore)
    GENERIC = "generic"  # Generic privacy compliance


class DataProcessingPurpose(str, Enum):
    """Legal purposes for data processing."""
    
    SERVICE_PROVISION = "service_provision"
    PERFORMANCE_IMPROVEMENT = "performance_improvement"
    SECURITY_MONITORING = "security_monitoring"
    LEGAL_COMPLIANCE = "legal_compliance"
    MARKETING = "marketing"
    ANALYTICS = "analytics"
    RESEARCH = "research"
    CUSTOMER_SUPPORT = "customer_support"


class ConsentStatus(str, Enum):
    """User consent status."""
    
    GRANTED = "granted"
    DENIED = "denied"
    WITHDRAWN = "withdrawn"
    PENDING = "pending"
    EXPIRED = "expired"


class DataCategory(str, Enum):
    """Categories of personal data processed."""
    
    IDENTITY_DATA = "identity_data"  # Name, ID numbers
    CONTACT_DATA = "contact_data"  # Email, phone, address
    DEMOGRAPHIC_DATA = "demographic_data"  # Age, gender, location
    FINANCIAL_DATA = "financial_data"  # Payment info, financial status
    TECHNICAL_DATA = "technical_data"  # IP addresses, device info
    USAGE_DATA = "usage_data"  # How services are used
    BEHAVIORAL_DATA = "behavioral_data"  # Preferences, interactions
    BIOMETRIC_DATA = "biometric_data"  # Fingerprints, facial recognition
    HEALTH_DATA = "health_data"  # Medical information
    SENSITIVE_DATA = "sensitive_data"  # Special categories


@dataclass
class DataProcessingRecord:
    """Record of data processing activity for compliance."""
    
    record_id: str
    timestamp: datetime
    
    # Data Subject Information
    data_subject_id: str
    data_subject_type: str  # user, customer, employee, etc.
    
    # Processing Details
    processing_purpose: DataProcessingPurpose
    data_categories: List[DataCategory]
    processing_description: str
    
    # Legal Basis
    legal_basis: str  # consent, contract, legal_obligation, etc.
    consent_status: Optional[ConsentStatus] = None
    consent_timestamp: Optional[datetime] = None
    
    # Data Flow
    data_source: str
    data_recipient: Optional[str] = None
    data_location: str  # geographic location
    cross_border_transfer: bool = False
    
    # Retention
    retention_period: Optional[int] = None  # days
    deletion_date: Optional[datetime] = None
    
    # Security
    encryption_used: bool = False
    anonymization_applied: bool = False
    pseudonymization_applied: bool = False
    
    # System Context
    system_component: str  # aphrodite_engine, echo_system, etc.
    request_id: Optional[str] = None
    session_id: Optional[str] = None
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for storage."""
        return {
            "record_id": self.record_id,
            "timestamp": self.timestamp.isoformat(),
            "data_subject_id": self.data_subject_id,
            "data_subject_type": self.data_subject_type,
            "processing_purpose": self.processing_purpose.value,
            "data_categories": [cat.value for cat in self.data_categories],
            "processing_description": self.processing_description,
            "legal_basis": self.legal_basis,
            "consent_status": self.consent_status.value if self.consent_status else None,
            "consent_timestamp": self.consent_timestamp.isoformat() if self.consent_timestamp else None,
            "data_source": self.data_source,
            "data_recipient": self.data_recipient,
            "data_location": self.data_location,
            "cross_border_transfer": self.cross_border_transfer,
            "retention_period": self.retention_period,
            "deletion_date": self.deletion_date.isoformat() if self.deletion_date else None,
            "encryption_used": self.encryption_used,
            "anonymization_applied": self.anonymization_applied,
            "pseudonymization_applied": self.pseudonymization_applied,
            "system_component": self.system_component,
            "request_id": self.request_id,
            "session_id": self.session_id
        }


class ConsentManager:
    """Manages user consent for data processing."""
    
    def __init__(self):
        """Initialize consent manager."""
        self.consent_records = {}  # user_id -> consent_info
        self.audit_logger = get_audit_logger()
    
    async def record_consent(
        self,
        user_id: str,
        purposes: List[DataProcessingPurpose],
        status: ConsentStatus = ConsentStatus.GRANTED,
        consent_method: str = "web_form",
        ip_address: str = None,
        user_agent: str = None
    ) -> str:
        """Record user consent for data processing."""
        
        consent_id = f"consent_{uuid.uuid4().hex[:12]}"
        timestamp = datetime.utcnow()
        
        consent_record = {
            "consent_id": consent_id,
            "user_id": user_id,
            "purposes": [p.value for p in purposes],
            "status": status.value,
            "timestamp": timestamp.isoformat(),
            "consent_method": consent_method,
            "ip_address": ip_address,
            "user_agent": user_agent,
            "expires_at": None  # Set if consent has expiration
        }
        
        # Store consent record
        if user_id not in self.consent_records:
            self.consent_records[user_id] = []
        self.consent_records[user_id].append(consent_record)
        
        # Audit log the consent
        await self.audit_logger.log_event(
            event_type=AuditEventType.DATA_ACCESS,
            message=f"User consent recorded: {status.value} for {len(purposes)} purposes",
            severity=AuditSeverity.MEDIUM,
            user_id=user_id,
            client_ip=ip_address,
            details={
                "consent_id": consent_id,
                "purposes": [p.value for p in purposes],
                "consent_method": consent_method
            }
        )
        
        return consent_id
    
    def check_consent(
        self,
        user_id: str,
        purpose: DataProcessingPurpose
    ) -> bool:
        """Check if user has valid consent for specific purpose."""
        
        if user_id not in self.consent_records:
            return False
        
        user_consents = self.consent_records[user_id]
        
        # Find most recent consent for this purpose
        for consent in reversed(user_consents):
            if purpose.value in consent.get("purposes", []):
                if consent["status"] == ConsentStatus.GRANTED.value:
                    # Check expiration if set
                    expires_at = consent.get("expires_at")
                    if expires_at:
                        expiry = datetime.fromisoformat(expires_at)
                        if datetime.utcnow() > expiry:
                            return False
                    return True
                else:
                    return False
        
        return False
    
    async def withdraw_consent(
        self,
        user_id: str,
        purposes: List[DataProcessingPurpose] = None,
        ip_address: str = None
    ) -> str:
        """Withdraw user consent for specified or all purposes."""
        
        if purposes is None:
            # Withdraw all consent
            all_purposes = []
            if user_id in self.consent_records:
                for consent in self.consent_records[user_id]:
                    all_purposes.extend(consent.get("purposes", []))
            purposes = list(set(all_purposes))
        
        # Record withdrawal
        withdrawal_id = await self.record_consent(
            user_id=user_id,
            purposes=purposes,
            status=ConsentStatus.WITHDRAWN,
            consent_method="withdrawal_request",
            ip_address=ip_address
        )
        
        return withdrawal_id


class DataRetentionManager:
    """Manages data retention and deletion policies."""
    
    def __init__(self):
        """Initialize retention manager."""
        self.retention_policies = {}  # data_category -> retention_days
        self.deletion_queue = []
        self.audit_logger = get_audit_logger()
        
        # Default retention policies (in days)
        self.default_policies = {
            DataCategory.USAGE_DATA: 90,
            DataCategory.TECHNICAL_DATA: 30,
            DataCategory.BEHAVIORAL_DATA: 180,
            DataCategory.CONTACT_DATA: 2555,  # 7 years
            DataCategory.IDENTITY_DATA: 2555,  # 7 years
            DataCategory.FINANCIAL_DATA: 2555,  # 7 years
            DataCategory.HEALTH_DATA: 3650,  # 10 years
            DataCategory.SENSITIVE_DATA: 365   # 1 year
        }
        
        self.retention_policies.update(self.default_policies)
    
    def set_retention_policy(
        self,
        data_category: DataCategory,
        retention_days: int
    ):
        """Set retention policy for data category."""
        self.retention_policies[data_category] = retention_days
        
        privacy_logger.info(
            f"Updated retention policy: {data_category.value} -> {retention_days} days"
        )
    
    def get_deletion_date(
        self,
        data_category: DataCategory,
        creation_date: datetime = None
    ) -> datetime:
        """Calculate deletion date for data category."""
        
        if creation_date is None:
            creation_date = datetime.utcnow()
        
        retention_days = self.retention_policies.get(
            data_category, 
            self.default_policies.get(data_category, 365)
        )
        
        return creation_date + timedelta(days=retention_days)
    
    async def schedule_deletion(
        self,
        data_identifier: str,
        data_category: DataCategory,
        deletion_date: datetime = None,
        reason: str = "retention_policy"
    ) -> str:
        """Schedule data for deletion."""
        
        if deletion_date is None:
            deletion_date = self.get_deletion_date(data_category)
        
        deletion_id = f"del_{uuid.uuid4().hex[:12]}"
        
        deletion_record = {
            "deletion_id": deletion_id,
            "data_identifier": data_identifier,
            "data_category": data_category.value,
            "scheduled_date": deletion_date.isoformat(),
            "reason": reason,
            "status": "scheduled",
            "created_at": datetime.utcnow().isoformat()
        }
        
        self.deletion_queue.append(deletion_record)
        
        # Audit log
        await self.audit_logger.log_event(
            event_type=AuditEventType.DATA_RETENTION_POLICY_APPLIED,
            message=f"Data deletion scheduled: {data_identifier}",
            severity=AuditSeverity.MEDIUM,
            details={
                "deletion_id": deletion_id,
                "data_category": data_category.value,
                "scheduled_date": deletion_date.isoformat(),
                "reason": reason
            }
        )
        
        return deletion_id
    
    async def process_deletions(self) -> int:
        """Process scheduled deletions that are due."""
        
        now = datetime.utcnow()
        processed_count = 0
        
        for deletion_record in self.deletion_queue[:]:  # Create copy for iteration
            scheduled_date = datetime.fromisoformat(deletion_record["scheduled_date"])
            
            if now >= scheduled_date and deletion_record["status"] == "scheduled":
                # Mark as processed (actual deletion would be implemented per system)
                deletion_record["status"] = "processed"
                deletion_record["processed_at"] = now.isoformat()
                
                # Audit log the deletion
                await self.audit_logger.log_event(
                    event_type=AuditEventType.DATA_DELETION,
                    message=f"Data deleted: {deletion_record['data_identifier']}",
                    severity=AuditSeverity.HIGH,
                    details={
                        "deletion_id": deletion_record["deletion_id"],
                        "data_category": deletion_record["data_category"],
                        "reason": deletion_record["reason"]
                    }
                )
                
                processed_count += 1
        
        # Remove processed items from queue
        self.deletion_queue = [
            record for record in self.deletion_queue 
            if record["status"] != "processed"
        ]
        
        return processed_count


class PrivacyComplianceManager:
    """
    Main privacy and compliance management system.
    
    Handles GDPR, CCPA and other privacy regulations with comprehensive
    data processing tracking, consent management, and compliance reporting.
    """
    
    def __init__(self, regulation: PrivacyRegulation = PrivacyRegulation.GDPR):
        """Initialize privacy compliance manager."""
        self.regulation = regulation
        self.audit_logger = get_audit_logger()
        self.consent_manager = ConsentManager()
        self.retention_manager = DataRetentionManager()
        
        # Processing records storage
        self.processing_records = []
        self.data_subject_registry = {}  # subject_id -> subject_info
        
        # Compliance configuration
        self.config = self._get_regulation_config(regulation)
        
        privacy_logger.info(f"Privacy compliance manager initialized for {regulation.value}")
    
    def _get_regulation_config(self, regulation: PrivacyRegulation) -> Dict[str, Any]:
        """Get configuration for specific privacy regulation."""
        
        configs = {
            PrivacyRegulation.GDPR: {
                "requires_consent": True,
                "consent_can_be_withdrawn": True,
                "right_to_access": True,
                "right_to_rectification": True,
                "right_to_erasure": True,
                "right_to_portability": True,
                "right_to_object": True,
                "data_protection_officer_required": False,  # Depends on organization
                "breach_notification_hours": 72,
                "max_fine_percentage": 4.0,  # 4% of annual turnover
                "territorial_scope": ["EU", "EEA"]
            },
            PrivacyRegulation.CCPA: {
                "requires_consent": False,  # Opt-out model
                "consent_can_be_withdrawn": True,
                "right_to_access": True,
                "right_to_rectification": False,
                "right_to_erasure": True,
                "right_to_portability": False,
                "right_to_object": True,
                "data_protection_officer_required": False,
                "breach_notification_hours": None,  # No specific timeframe
                "max_fine_per_violation": 7500,
                "territorial_scope": ["California"]
            }
        }
        
        return configs.get(regulation, configs[PrivacyRegulation.GDPR])
    
    async def record_data_processing(
        self,
        data_subject_id: str,
        purpose: DataProcessingPurpose,
        data_categories: List[DataCategory],
        description: str,
        legal_basis: str = "legitimate_interest",
        request: Request = None,
        **kwargs
    ) -> str:
        """Record a data processing activity."""
        
        record_id = f"proc_{uuid.uuid4().hex[:12]}"
        
        # Extract context from request if provided
        context = {}
        if request:
            context.update({
                "request_id": getattr(request.state, 'request_id', None),
                "session_id": getattr(request.state, 'session_id', None),
                "data_source": f"api:{request.url.path}",
                "data_location": "eu-west-1"  # Configure based on deployment
            })
        
        # Create processing record
        processing_record = DataProcessingRecord(
            record_id=record_id,
            timestamp=datetime.utcnow(),
            data_subject_id=data_subject_id,
            data_subject_type="user",
            processing_purpose=purpose,
            data_categories=data_categories,
            processing_description=description,
            legal_basis=legal_basis,
            system_component="aphrodite_engine",
            **context,
            **kwargs
        )
        
        # Check consent if required
        if self.config.get("requires_consent") and legal_basis == "consent":
            has_consent = self.consent_manager.check_consent(data_subject_id, purpose)
            if not has_consent:
                processing_record.consent_status = ConsentStatus.DENIED
                
                # Audit log consent violation
                await self.audit_logger.log_event(
                    event_type=AuditEventType.SECURITY_POLICY_VIOLATION,
                    message=f"Data processing without consent: {data_subject_id}",
                    severity=AuditSeverity.HIGH,
                    user_id=data_subject_id,
                    details={
                        "processing_purpose": purpose.value,
                        "violation_type": "missing_consent"
                    }
                )
            else:
                processing_record.consent_status = ConsentStatus.GRANTED
        
        # Store processing record
        self.processing_records.append(processing_record)
        
        # Schedule automatic deletion based on retention policy
        for data_category in data_categories:
            await self.retention_manager.schedule_deletion(
                data_identifier=f"{data_subject_id}:{purpose.value}:{data_category.value}",
                data_category=data_category,
                reason="retention_policy"
            )
        
        # Audit log the processing
        await self.audit_logger.log_event(
            event_type=AuditEventType.DATA_ACCESS,
            message=f"Data processing recorded: {purpose.value}",
            severity=AuditSeverity.MEDIUM,
            user_id=data_subject_id,
            details={
                "record_id": record_id,
                "purpose": purpose.value,
                "data_categories": [cat.value for cat in data_categories],
                "legal_basis": legal_basis
            }
        )
        
        return record_id
    
    async def handle_subject_access_request(
        self,
        data_subject_id: str,
        request_type: str = "access"  # access, portability, erasure, rectification
    ) -> Dict[str, Any]:
        """Handle data subject rights requests (GDPR Article 15, etc.)."""
        
        # Find all processing records for this subject
        subject_records = [
            record for record in self.processing_records
            if record.data_subject_id == data_subject_id
        ]
        
        response = {
            "request_id": f"dsr_{uuid.uuid4().hex[:12]}",
            "data_subject_id": data_subject_id,
            "request_type": request_type,
            "timestamp": datetime.utcnow().isoformat(),
            "regulation": self.regulation.value
        }
        
        if request_type == "access":
            # Right to access (GDPR Art. 15)
            response.update({
                "processing_records": [record.to_dict() for record in subject_records],
                "data_categories_processed": list(set(
                    cat.value for record in subject_records 
                    for cat in record.data_categories
                )),
                "purposes_of_processing": list(set(
                    record.processing_purpose.value for record in subject_records
                )),
                "recipients": list(set(
                    record.data_recipient for record in subject_records
                    if record.data_recipient
                )),
                "retention_periods": {
                    cat.value: self.retention_manager.retention_policies.get(cat, "unknown")
                    for record in subject_records for cat in record.data_categories
                }
            })
        
        elif request_type == "erasure":
            # Right to erasure (GDPR Art. 17)
            if self.config.get("right_to_erasure"):
                deletion_ids = []
                for record in subject_records:
                    for category in record.data_categories:
                        deletion_id = await self.retention_manager.schedule_deletion(
                            data_identifier=f"{data_subject_id}:all_data",
                            data_category=category,
                            deletion_date=datetime.utcnow(),  # Immediate deletion
                            reason="subject_request"
                        )
                        deletion_ids.append(deletion_id)
                
                response.update({
                    "erasure_scheduled": True,
                    "deletion_ids": deletion_ids,
                    "records_affected": len(subject_records)
                })
        
        elif request_type == "portability":
            # Right to data portability (GDPR Art. 20)
            if self.config.get("right_to_portability"):
                portable_data = {}
                for record in subject_records:
                    if record.legal_basis == "consent":  # Only consented data is portable
                        for category in record.data_categories:
                            if category.value not in portable_data:
                                portable_data[category.value] = []
                            portable_data[category.value].append({
                                "purpose": record.processing_purpose.value,
                                "data_source": record.data_source,
                                "timestamp": record.timestamp.isoformat()
                            })
                
                response.update({
                    "portable_data": portable_data,
                    "export_format": "json",
                    "export_timestamp": datetime.utcnow().isoformat()
                })
        
        # Audit log the request
        await self.audit_logger.log_event(
            event_type=AuditEventType.DATA_ACCESS,
            message=f"Subject rights request processed: {request_type}",
            severity=AuditSeverity.HIGH,
            user_id=data_subject_id,
            details={
                "request_id": response["request_id"],
                "request_type": request_type,
                "records_found": len(subject_records)
            }
        )
        
        return response
    
    async def generate_privacy_report(
        self,
        start_date: datetime = None,
        end_date: datetime = None,
        report_type: str = "comprehensive"
    ) -> Dict[str, Any]:
        """Generate privacy compliance report."""
        
        if start_date is None:
            start_date = datetime.utcnow() - timedelta(days=30)
        if end_date is None:
            end_date = datetime.utcnow()
        
        # Filter records by date range
        period_records = [
            record for record in self.processing_records
            if start_date <= record.timestamp <= end_date
        ]
        
        # Aggregate statistics
        purpose_stats = {}
        category_stats = {}
        legal_basis_stats = {}
        
        for record in period_records:
            # Purpose statistics
            purpose = record.processing_purpose.value
            purpose_stats[purpose] = purpose_stats.get(purpose, 0) + 1
            
            # Category statistics
            for category in record.data_categories:
                cat_name = category.value
                category_stats[cat_name] = category_stats.get(cat_name, 0) + 1
            
            # Legal basis statistics
            basis = record.legal_basis
            legal_basis_stats[basis] = legal_basis_stats.get(basis, 0) + 1
        
        # Privacy metrics
        consent_granted = len([
            r for r in period_records 
            if r.consent_status == ConsentStatus.GRANTED
        ])
        consent_denied = len([
            r for r in period_records 
            if r.consent_status == ConsentStatus.DENIED
        ])
        
        # Generate report
        report = {
            "report_id": f"privacy_report_{uuid.uuid4().hex[:12]}",
            "report_type": report_type,
            "regulation": self.regulation.value,
            "generated_at": datetime.utcnow().isoformat(),
            "period": {
                "start": start_date.isoformat(),
                "end": end_date.isoformat()
            },
            "summary": {
                "total_processing_records": len(period_records),
                "unique_data_subjects": len(set(r.data_subject_id for r in period_records)),
                "consent_granted": consent_granted,
                "consent_denied": consent_denied,
                "consent_rate": (consent_granted / len(period_records)) * 100 if period_records else 0
            },
            "processing_statistics": {
                "by_purpose": purpose_stats,
                "by_data_category": category_stats,
                "by_legal_basis": legal_basis_stats
            },
            "retention_statistics": {
                "scheduled_deletions": len(self.retention_manager.deletion_queue),
                "retention_policies": dict(self.retention_manager.retention_policies)
            },
            "compliance_indicators": {
                "regulation_config": self.config,
                "privacy_by_design": True,  # Configure based on implementation
                "data_minimization": True,  # Assess based on actual data usage
                "purpose_limitation": True,  # Based on purpose tracking
                "accuracy": True,  # Based on data quality controls
                "storage_limitation": True,  # Based on retention policies
                "integrity_confidentiality": True  # Based on security measures
            }
        }
        
        # Add regulation-specific metrics
        if self.regulation == PrivacyRegulation.GDPR:
            report["gdpr_metrics"] = {
                "lawful_basis_distribution": legal_basis_stats,
                "special_category_processing": len([
                    r for r in period_records 
                    if DataCategory.SENSITIVE_DATA in r.data_categories
                ]),
                "cross_border_transfers": len([
                    r for r in period_records if r.cross_border_transfer
                ]),
                "automated_decision_making": 0  # Configure based on ML usage
            }
        
        return report
    
    async def assess_privacy_impact(
        self,
        processing_description: str,
        data_categories: List[DataCategory],
        purposes: List[DataProcessingPurpose],
        data_subjects_count: int = None
    ) -> Dict[str, Any]:
        """Conduct Privacy Impact Assessment (PIA/DPIA)."""
        
        assessment_id = f"pia_{uuid.uuid4().hex[:12]}"
        
        # Calculate risk scores
        risk_factors = {
            "sensitive_data": any(
                cat in [DataCategory.SENSITIVE_DATA, DataCategory.BIOMETRIC_DATA, 
                       DataCategory.HEALTH_DATA] 
                for cat in data_categories
            ),
            "large_scale": data_subjects_count and data_subjects_count > 10000,
            "automated_processing": "automated" in processing_description.lower(),
            "profiling": any(
                purpose in [DataProcessingPurpose.ANALYTICS, DataProcessingPurpose.MARKETING]
                for purpose in purposes
            ),
            "vulnerable_subjects": False,  # Configure based on context
            "new_technology": "ai" in processing_description.lower() or "ml" in processing_description.lower()
        }
        
        # Calculate overall risk score
        risk_score = sum(1 for factor in risk_factors.values() if factor)
        max_risk = len(risk_factors)
        risk_percentage = (risk_score / max_risk) * 100
        
        # Determine risk level
        if risk_percentage >= 75:
            risk_level = "HIGH"
        elif risk_percentage >= 50:
            risk_level = "MEDIUM"
        elif risk_percentage >= 25:
            risk_level = "LOW"
        else:
            risk_level = "MINIMAL"
        
        # Generate recommendations
        recommendations = []
        if risk_factors["sensitive_data"]:
            recommendations.append("Implement additional encryption for sensitive data")
        if risk_factors["large_scale"]:
            recommendations.append("Conduct regular compliance audits")
        if risk_factors["automated_processing"]:
            recommendations.append("Implement human oversight for automated decisions")
        if risk_factors["new_technology"]:
            recommendations.append("Regular privacy impact assessments for AI/ML systems")
        
        assessment = {
            "assessment_id": assessment_id,
            "timestamp": datetime.utcnow().isoformat(),
            "processing_description": processing_description,
            "data_categories": [cat.value for cat in data_categories],
            "purposes": [purpose.value for purpose in purposes],
            "data_subjects_count": data_subjects_count,
            "risk_assessment": {
                "risk_factors": risk_factors,
                "risk_score": risk_score,
                "max_risk": max_risk,
                "risk_percentage": risk_percentage,
                "risk_level": risk_level
            },
            "recommendations": recommendations,
            "requires_dpo_review": risk_level in ["HIGH", "MEDIUM"],
            "requires_authority_consultation": risk_level == "HIGH"
        }
        
        # Audit log the assessment
        await self.audit_logger.log_event(
            event_type=AuditEventType.DATA_ACCESS,
            message=f"Privacy impact assessment completed: {risk_level} risk",
            severity=AuditSeverity.MEDIUM if risk_level in ["LOW", "MINIMAL"] else AuditSeverity.HIGH,
            details={
                "assessment_id": assessment_id,
                "risk_level": risk_level,
                "risk_percentage": risk_percentage
            }
        )
        
        return assessment
    
    async def handle_data_breach(
        self,
        breach_description: str,
        data_categories_affected: List[DataCategory],
        estimated_subjects_affected: int,
        breach_severity: str = "medium",
        containment_measures: List[str] = None
    ) -> Dict[str, Any]:
        """Handle data breach notification and compliance requirements."""
        
        breach_id = f"breach_{uuid.uuid4().hex[:12]}"
        detected_at = datetime.utcnow()
        
        # Determine notification requirements
        notification_requirements = {
            "authority_notification_required": breach_severity in ["high", "critical"],
            "subject_notification_required": (
                breach_severity in ["high", "critical"] or
                any(cat in [DataCategory.SENSITIVE_DATA, DataCategory.FINANCIAL_DATA] 
                    for cat in data_categories_affected)
            ),
            "notification_deadline": detected_at + timedelta(hours=self.config.get("breach_notification_hours", 72))
        }
        
        breach_record = {
            "breach_id": breach_id,
            "detected_at": detected_at.isoformat(),
            "description": breach_description,
            "severity": breach_severity,
            "data_categories_affected": [cat.value for cat in data_categories_affected],
            "estimated_subjects_affected": estimated_subjects_affected,
            "containment_measures": containment_measures or [],
            "notification_requirements": notification_requirements,
            "status": "detected"
        }
        
        # Audit log the breach
        await self.audit_logger.log_event(
            event_type=AuditEventType.SECURITY_ATTACK_ATTEMPT,
            message=f"Data breach detected: {breach_severity} severity",
            severity=AuditSeverity.CRITICAL,
            details={
                "breach_id": breach_id,
                "affected_subjects": estimated_subjects_affected,
                "data_categories": [cat.value for cat in data_categories_affected]
            }
        )
        
        return breach_record


# Global privacy compliance manager
_global_privacy_manager: Optional[PrivacyComplianceManager] = None


def get_privacy_manager(regulation: PrivacyRegulation = PrivacyRegulation.GDPR) -> PrivacyComplianceManager:
    """Get global privacy compliance manager."""
    global _global_privacy_manager
    
    if _global_privacy_manager is None:
        _global_privacy_manager = PrivacyComplianceManager(regulation)
    
    return _global_privacy_manager


def configure_privacy_compliance(regulation: PrivacyRegulation):
    """Configure global privacy compliance."""
    global _global_privacy_manager
    _global_privacy_manager = PrivacyComplianceManager(regulation)