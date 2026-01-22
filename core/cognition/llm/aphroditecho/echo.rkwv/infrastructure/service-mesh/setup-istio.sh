#!/bin/bash
set -euo pipefail
ISTIO_VERSION="${ISTIO_VERSION:-1.20.0}"
NAMESPACE="${NAMESPACE:-istio-system}"
CONFIG_DIR="infrastructure/service-mesh"
log() {
echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}
error() {
echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: $1" >&2
exit 1
}
check_prerequisites() {
log "Checking prerequisites..."
command -v kubectl >/dev/null 2>&1 || error "kubectl not found"
command -v helm >/dev/null 2>&1 || error "helm not found"
kubectl cluster-info >/dev/null 2>&1 || error "Kubernetes cluster not accessible"
log "Prerequisites check passed"
}
install_istio() {
log "Installing Istio $ISTIO_VERSION..."
if [ ! -d "istio-$ISTIO_VERSION" ]; then
curl -L https://istio.io/downloadIstio | ISTIO_VERSION=$ISTIO_VERSION sh -
fi
export PATH="$PWD/istio-$ISTIO_VERSION/bin:$PATH"
istioctl version --remote=false
log "Istio downloaded successfully"
}
install_control_plane() {
log "Installing Istio control plane..."
kubectl create namespace $NAMESPACE --dry-run=client -o yaml | kubectl apply -f -
istioctl install -f "$CONFIG_DIR/istio-config.yaml" -y
kubectl wait --for=condition=ready pod -l app=istiod -n $NAMESPACE --timeout=300s
log "Istio control plane installed successfully"
}
configure_injection() {
log "Configuring namespace injection..."
kubectl label namespace default istio-injection=enabled --overwrite
kubectl get namespace deep-echo >/dev/null 2>&1 && \
kubectl label namespace deep-echo istio-injection=enabled --overwrite || true
log "Namespace injection configured"
}
install_addons() {
log "Installing Istio addons..."
local addons_dir="istio-$ISTIO_VERSION/samples/addons"
kubectl apply -f "$addons_dir/kiali.yaml" || log "Warning: Failed to install Kiali"
kubectl apply -f "$addons_dir/jaeger.yaml" || log "Warning: Failed to install Jaeger"
kubectl apply -f "$addons_dir/prometheus.yaml" || log "Warning: Failed to install Prometheus"
kubectl apply -f "$addons_dir/grafana.yaml" || log "Warning: Failed to install Grafana"
log "Addons installation completed"
}
apply_deep_echo_config() {
log "Applying Deep Echo service mesh configuration..."
kubectl apply -f "$CONFIG_DIR/istio-config.yaml"
kubectl wait --for=condition=programmed gateway/deep-echo-gateway --timeout=300s || \
log "Warning: Gateway may not be ready yet"
log "Deep Echo service mesh configuration applied"
}
configure_traffic_management() {
log "Configuring traffic management..."
cat << EOF | kubectl apply -f -
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
name: deep-echo-canary
namespace: default
spec:
hosts:
- deep-echo-cognitive-svc
http:
- match:
- headers:
canary:
exact: "true"
route:
- destination:
host: deep-echo-cognitive-svc
subset: canary
- route:
- destination:
host: deep-echo-cognitive-svc
subset: v1
weight: 90
- destination:
host: deep-echo-cognitive-svc
subset: canary
weight: 10
EOF
log "Traffic management configured"
}
verify_installation() {
log "Verifying Istio installation..."
istioctl verify-install
kubectl get pods -n default -o wide
kubectl get svc -n $NAMESPACE
local gateway_ip=$(kubectl get svc istio-ingressgateway -n $NAMESPACE -o jsonpath='{.status.loadBalancer.ingress[0].ip}')
if [ -n "$gateway_ip" ] && [ "$gateway_ip" != "null" ]; then
log "Gateway IP: $gateway_ip"
else
log "Gateway IP not yet assigned"
fi
log "Istio installation verified"
}
generate_test_traffic() {
log "Generating test traffic..."
kubectl port-forward -n $NAMESPACE svc/istio-ingressgateway 8080:80 &
local port_forward_pid=$!
sleep 5
for i in {1..10}; do
curl -s http://localhost:8080/api/status >/dev/null || true
curl -s http://localhost:8080/ >/dev/null || true
sleep 1
done
kill $port_forward_pid 2>/dev/null || true
log "Test traffic generated"
}
show_access_info() {
log "Service mesh access information:"
echo
echo "=========================================="
echo "Istio Service Mesh Access Information"
echo "=========================================="
echo "Gateway:"
kubectl get gateway -o wide
echo
echo "VirtualServices:"
kubectl get virtualservice -o wide
echo
echo "DestinationRules:"
kubectl get destinationrule -o wide
echo
echo "Addon Access (use kubectl port-forward):"
echo "- Kiali: kubectl port-forward -n istio-system svc/kiali 20001:20001"
echo "- Jaeger: kubectl port-forward -n istio-system svc/tracing 16686:80"
echo "- Grafana: kubectl port-forward -n istio-system svc/grafana 3000:3000"
echo "- Prometheus: kubectl port-forward -n istio-system svc/prometheus 9090:9090"
echo "=========================================="
}
cleanup() {
log "Cleaning up Istio installation..."
kubectl delete -f "$CONFIG_DIR/istio-config.yaml" 2>/dev/null || true
istioctl uninstall --purge -y
kubectl delete namespace $NAMESPACE --ignore-not-found
kubectl label namespace default istio-injection- || true
log "Cleanup completed"
}
main() {
case "${1:-install}" in
"install")
check_prerequisites
install_istio
install_control_plane
configure_injection
install_addons
apply_deep_echo_config
configure_traffic_management
verify_installation
generate_test_traffic
show_access_info
;;
"config")
apply_deep_echo_config
configure_traffic_management
;;
"verify")
verify_installation
;;
"cleanup")
cleanup
;;
"traffic")
generate_test_traffic
;;
"info")
show_access_info
;;
*)
echo "Usage: $0 [install|config|verify|cleanup|traffic|info]"
echo
echo "Commands:"
echo "  install  - Full Istio installation and configuration"
echo "  config   - Apply Deep Echo service mesh configuration"
echo "  verify   - Verify Istio installation"
echo "  cleanup  - Remove Istio installation"
echo "  traffic  - Generate test traffic"
echo "  info     - Show access information"
exit 1
;;
esac
}
main "$@"