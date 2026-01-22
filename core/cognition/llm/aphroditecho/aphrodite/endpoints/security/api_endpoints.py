from datetime import datetime
from typing import Any, Dict, List, Optional
from fastapi import APIRouter, Depends, HTTPException, Query, Body, Security
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field
import structlog
from .audit_logger import get_audit_logger, AuditEventType, AuditSeverity
from .privacy_compliance import get_privacy_manager, DataProcessingPurpose, DataCategory
from .incident_response import get_incident_engine, IncidentStatus
security_api_logger = structlog.get_logger('security_api')
security_scheme = HTTPBearer()
security_router = APIRouter(prefix='/security', tags=['Security & Compliance'])
class AuditLogQuery(BaseModel):
    start_time: Optional[datetime] = Field(None, description='Start time for query')
    end_time: Optional[datetime] = Field(None, description='End time for query')
    event_types: Optional[List[str]] = Field(None, description='Filter by event types')
    user_id: Optional[str] = Field(None, description='Filter by user ID')
    client_ip: Optional[str] = Field(None, description='Filter by client IP')
    severity: Optional[str] = Field(None, description='Filter by severity level')
    limit: int = Field(100, ge=1, le=1000, description='Maximum number of records')
class ComplianceReportRequest(BaseModel):
    start_date: datetime = Field(..., description='Report start date')
    end_date: datetime = Field(..., description='Report end date')
    report_type: str = Field('comprehensive', description='Type of report to generate')
    include_details: bool = Field(False, description='Include detailed event records')
class DataSubjectRequest(BaseModel):
    data_subject_id: str = Field(..., description='ID of the data subject')
    request_type: str = Field(..., description='Type of request (access, erasure, portability, rectification)')
    contact_email: Optional[str] = Field(None, description='Contact email for response')
    justification: Optional[str] = Field(None, description='Justification for the request')
class IncidentUpdateRequest(BaseModel):
    status: str = Field(..., description='New incident status')
    resolution_notes: Optional[str] = Field(None, description='Resolution notes')
    assigned_to: Optional[str] = Field(None, description='Person assigned to incident')
class PrivacyImpactAssessmentRequest(BaseModel):
    processing_description: str = Field(..., description='Description of data processing')
    data_categories: List[str] = Field(..., description='Categories of data being processed')
    purposes: List[str] = Field(..., description='Purposes of processing')
    data_subjects_count: Optional[int] = Field(None, description='Estimated number of data subjects')
async def verify_security_admin(credentials: HTTPAuthorizationCredentials=Security(security_scheme)) -> str:
    if not credentials or not credentials.credentials:
        raise HTTPException(status_code=401, detail='Authentication required for security endpoints')
    audit_logger = get_audit_logger()
    await audit_logger.log_event(event_type=AuditEventType.AUTH_ACCESS_DENIED if not credentials.credentials else AuditEventType.DATA_ACCESS, message='Security API access attempt', severity=AuditSeverity.HIGH, details={'endpoint': 'security_api', 'token_provided': bool(credentials.credentials)})
    return 'security_admin'
@security_router.get('/audit/logs', summary='Query audit logs')
async def query_audit_logs(query: AuditLogQuery=Depends(), admin: str=Depends(verify_security_admin)) -> Dict[str, Any]:
    try:
        audit_logger = get_audit_logger()
        event_types = None
        if query.event_types:
            event_types = []
            for event_type_str in query.event_types:
                try:
                    event_types.append(AuditEventType(event_type_str))
                except ValueError:
                    pass
        events = await audit_logger.query_events(start_time=query.start_time, end_time=query.end_time, event_types=event_types, user_id=query.user_id, limit=query.limit)
        await audit_logger.log_event(event_type=AuditEventType.DATA_ACCESS, message=f'Audit logs queried by {admin}', severity=AuditSeverity.MEDIUM, details={'query_parameters': query.dict(), 'results_count': len(events)})
        return {'status': 'success', 'query': query.dict(), 'total_events': len(events), 'events': events}
    except Exception as e:
        security_api_logger.error(f'Error querying audit logs: {e}')
        raise HTTPException(status_code=500, detail='Failed to query audit logs')
@security_router.post('/audit/events', summary='Create audit event')
async def create_audit_event(event_type: str=Body(..., description='Audit event type'), message: str=Body(..., description='Event message'), severity: str=Body('medium', description='Event severity'), details: Dict[str, Any]=Body(None, description='Additional event details'), admin: str=Depends(verify_security_admin)) -> Dict[str, Any]:
    try:
        audit_logger = get_audit_logger()
        try:
            audit_event_type = AuditEventType(event_type)
        except ValueError:
            raise HTTPException(status_code=400, detail=f'Invalid event type: {event_type}')
        try:
            audit_severity = AuditSeverity(severity)
        except ValueError:
            raise HTTPException(status_code=400, detail=f'Invalid severity: {severity}')
        event_id = await audit_logger.log_event(event_type=audit_event_type, message=message, severity=audit_severity, details=details or {}, user_id=admin)
        return {'status': 'success', 'event_id': event_id, 'message': 'Audit event created successfully'}
    except HTTPException:
        raise
    except Exception as e:
        security_api_logger.error(f'Error creating audit event: {e}')
        raise HTTPException(status_code=500, detail='Failed to create audit event')
@security_router.post('/compliance/report', summary='Generate compliance report')
async def generate_compliance_report(request: ComplianceReportRequest, admin: str=Depends(verify_security_admin)) -> Dict[str, Any]:
    try:
        audit_logger = get_audit_logger()
        privacy_manager = get_privacy_manager()
        incident_engine = get_incident_engine()
        audit_report = await audit_logger.generate_compliance_report(request.start_date, request.end_date, request.report_type)
        privacy_report = await privacy_manager.generate_privacy_report(request.start_date, request.end_date, request.report_type)
        security_dashboard = await incident_engine.get_security_dashboard()
        compliance_report = {'report_metadata': {'generated_at': datetime.utcnow().isoformat(), 'generated_by': admin, 'period': {'start': request.start_date.isoformat(), 'end': request.end_date.isoformat()}, 'report_type': request.report_type}, 'audit_compliance': audit_report, 'privacy_compliance': privacy_report, 'security_status': security_dashboard, 'overall_compliance': {'audit_events_total': audit_report.get('summary', {}).get('total_events', 0), 'privacy_records_total': privacy_report.get('summary', {}).get('total_processing_records', 0), 'security_incidents_total': security_dashboard.get('summary', {}).get('total_active_incidents', 0)}}
        await audit_logger.log_event(event_type=AuditEventType.DATA_ACCESS, message=f'Compliance report generated by {admin}', severity=AuditSeverity.HIGH, details={'report_type': request.report_type, 'period_days': (request.end_date - request.start_date).days})
        return compliance_report
    except Exception as e:
        security_api_logger.error(f'Error generating compliance report: {e}')
        raise HTTPException(status_code=500, detail='Failed to generate compliance report')
@security_router.post('/privacy/data-subject-request', summary='Handle data subject request')
async def handle_data_subject_request(request: DataSubjectRequest, admin: str=Depends(verify_security_admin)) -> Dict[str, Any]:
    try:
        privacy_manager = get_privacy_manager()
        response = await privacy_manager.handle_subject_access_request(data_subject_id=request.data_subject_id, request_type=request.request_type)
        audit_logger = get_audit_logger()
        await audit_logger.log_event(event_type=AuditEventType.DATA_ACCESS, message=f'Data subject request handled: {request.request_type}', severity=AuditSeverity.HIGH, user_id=request.data_subject_id, details={'request_type': request.request_type, 'handled_by': admin, 'contact_email': request.contact_email})
        return {'status': 'success', 'request_id': response['request_id'], 'response': response, 'message': f'Data subject {request.request_type} request processed successfully'}
    except Exception as e:
        security_api_logger.error(f'Error handling data subject request: {e}')
        raise HTTPException(status_code=500, detail='Failed to handle data subject request')
@security_router.post('/privacy/impact-assessment', summary='Conduct privacy impact assessment')
async def conduct_privacy_impact_assessment(request: PrivacyImpactAssessmentRequest, admin: str=Depends(verify_security_admin)) -> Dict[str, Any]:
    try:
        privacy_manager = get_privacy_manager()
        data_categories = []
        for category_str in request.data_categories:
            try:
                data_categories.append(DataCategory(category_str))
            except ValueError:
                pass
        purposes = []
        for purpose_str in request.purposes:
            try:
                purposes.append(DataProcessingPurpose(purpose_str))
            except ValueError:
                pass
        assessment = await privacy_manager.assess_privacy_impact(processing_description=request.processing_description, data_categories=data_categories, purposes=purposes, data_subjects_count=request.data_subjects_count)
        audit_logger = get_audit_logger()
        await audit_logger.log_event(event_type=AuditEventType.DATA_ACCESS, message=f'Privacy impact assessment conducted by {admin}', severity=AuditSeverity.MEDIUM, details={'assessment_id': assessment['assessment_id'], 'risk_level': assessment['risk_assessment']['risk_level']})
        return {'status': 'success', 'assessment': assessment, 'message': 'Privacy impact assessment completed successfully'}
    except Exception as e:
        security_api_logger.error(f'Error conducting privacy assessment: {e}')
        raise HTTPException(status_code=500, detail='Failed to conduct privacy impact assessment')
@security_router.get('/incidents', summary='List security incidents')
async def list_security_incidents(status: Optional[str]=Query(None, description='Filter by incident status'), severity: Optional[str]=Query(None, description='Filter by severity'), limit: int=Query(50, ge=1, le=200, description='Maximum number of incidents'), admin: str=Depends(verify_security_admin)) -> Dict[str, Any]:
    try:
        incident_engine = get_incident_engine()
        dashboard = await incident_engine.get_security_dashboard()
        incidents = []
        for incident in incident_engine.active_incidents.values():
            if status and incident.status.value != status:
                continue
            if severity and incident.severity.value != severity:
                continue
            incident_dict = {'incident_id': incident.incident_id, 'title': incident.title, 'threat_type': incident.threat_type.value, 'severity': incident.severity.value, 'status': incident.status.value, 'detected_at': incident.detected_at.isoformat(), 'last_updated': incident.last_updated.isoformat(), 'events_count': len(incident.events), 'response_actions': [action.value for action in incident.response_actions], 'estimated_impact': incident.estimated_impact}
            incidents.append(incident_dict)
            if len(incidents) >= limit:
                break
        audit_logger = get_audit_logger()
        await audit_logger.log_event(event_type=AuditEventType.DATA_ACCESS, message=f'Security incidents queried by {admin}', severity=AuditSeverity.MEDIUM, details={'filters': {'status': status, 'severity': severity}, 'results_count': len(incidents)})
        return {'status': 'success', 'total_incidents': len(incidents), 'incidents': incidents, 'dashboard_summary': dashboard.get('summary', {})}
    except Exception as e:
        security_api_logger.error(f'Error listing security incidents: {e}')
        raise HTTPException(status_code=500, detail='Failed to list security incidents')
@security_router.get('/incidents/{incident_id}', summary='Get incident details')
async def get_incident_details(incident_id: str, admin: str=Depends(verify_security_admin)) -> Dict[str, Any]:
    try:
        incident_engine = get_incident_engine()
        incident_status = await incident_engine.get_incident_status(incident_id)
        if not incident_status:
            raise HTTPException(status_code=404, detail='Incident not found')
        incident = incident_engine.active_incidents.get(incident_id)
        if not incident:
            raise HTTPException(status_code=404, detail='Incident details not found')
        incident_details = {'incident_id': incident.incident_id, 'title': incident.title, 'description': incident.description, 'threat_type': incident.threat_type.value, 'severity': incident.severity.value, 'status': incident.status.value, 'detected_at': incident.detected_at.isoformat(), 'last_updated': incident.last_updated.isoformat(), 'resolved_at': incident.resolved_at.isoformat() if incident.resolved_at else None, 'affected_assets': incident.affected_assets, 'estimated_impact': incident.estimated_impact, 'affected_users': incident.affected_users, 'data_at_risk': incident.data_at_risk, 'indicators_of_compromise': incident.indicators_of_compromise, 'response_actions': [action.value for action in incident.response_actions], 'assigned_to': incident.assigned_to, 'resolution_notes': incident.resolution_notes, 'events': [{'event_id': event.event_id, 'timestamp': event.timestamp.isoformat(), 'source_ip': event.source_ip, 'endpoint': event.endpoint, 'description': event.description, 'anomaly_score': event.anomaly_score} for event in incident.events]}
        audit_logger = get_audit_logger()
        await audit_logger.log_event(event_type=AuditEventType.DATA_ACCESS, message=f'Security incident details accessed by {admin}', severity=AuditSeverity.MEDIUM, details={'incident_id': incident_id})
        return {'status': 'success', 'incident': incident_details}
    except HTTPException:
        raise
    except Exception as e:
        security_api_logger.error(f'Error getting incident details: {e}')
        raise HTTPException(status_code=500, detail='Failed to get incident details')
@security_router.put('/incidents/{incident_id}', summary='Update incident status')
async def update_incident_status(incident_id: str, update: IncidentUpdateRequest, admin: str=Depends(verify_security_admin)) -> Dict[str, Any]:
    try:
        incident_engine = get_incident_engine()
        try:
            new_status = IncidentStatus(update.status)
        except ValueError:
            raise HTTPException(status_code=400, detail=f'Invalid status: {update.status}')
        success = await incident_engine.update_incident_status(incident_id=incident_id, new_status=new_status, resolution_notes=update.resolution_notes)
        if not success:
            raise HTTPException(status_code=404, detail='Incident not found')
        if update.assigned_to:
            incident = incident_engine.active_incidents.get(incident_id)
            if incident:
                incident.assigned_to = update.assigned_to
        audit_logger = get_audit_logger()
        await audit_logger.log_event(event_type=AuditEventType.SYSTEM_CONFIG_CHANGE, message=f'Incident status updated by {admin}', severity=AuditSeverity.HIGH, details={'incident_id': incident_id, 'new_status': update.status, 'updated_by': admin})
        return {'status': 'success', 'incident_id': incident_id, 'message': 'Incident status updated successfully'}
    except HTTPException:
        raise
    except Exception as e:
        security_api_logger.error(f'Error updating incident status: {e}')
        raise HTTPException(status_code=500, detail='Failed to update incident status')
@security_router.get('/status', summary='Get security system status')
async def get_security_system_status(admin: str=Depends(verify_security_admin)) -> Dict[str, Any]:
    try:
        audit_logger = get_audit_logger()
        privacy_manager = get_privacy_manager()
        incident_engine = get_incident_engine()
        security_dashboard = await incident_engine.get_security_dashboard()
        performance_metrics = getattr(incident_engine, 'performance_metrics', {})
        status = {'system_status': 'operational', 'timestamp': datetime.utcnow().isoformat(), 'components': {'audit_logger': 'operational' if audit_logger else 'unavailable', 'privacy_manager': 'operational' if privacy_manager else 'unavailable', 'incident_engine': 'operational' if incident_engine else 'unavailable'}, 'security_summary': security_dashboard.get('summary', {}), 'detector_status': security_dashboard.get('detector_performance', {}), 'system_performance': performance_metrics}
        return status
    except Exception as e:
        security_api_logger.error(f'Error getting system status: {e}')
        raise HTTPException(status_code=500, detail='Failed to get system status')
@security_router.get('/health', summary='Security system health check', include_in_schema=False)
async def security_health_check() -> Dict[str, Any]:
    try:
        audit_logger = get_audit_logger()
        incident_engine = get_incident_engine()
        health_status = {'status': 'healthy', 'timestamp': datetime.utcnow().isoformat(), 'components': {'audit_system': 'ok' if audit_logger else 'error', 'incident_response': 'ok' if incident_engine else 'error'}}
        if all((status == 'ok' for status in health_status['components'].values())):
            health_status['status'] = 'healthy'
        else:
            health_status['status'] = 'degraded'
        return health_status
    except Exception as e:
        security_api_logger.error(f'Health check error: {e}')
        return {'status': 'unhealthy', 'timestamp': datetime.utcnow().isoformat(), 'error': 'Health check failed'}
def include_security_routes(app):
    app.include_router(security_router)
    security_api_logger.info('Security API endpoints registered')