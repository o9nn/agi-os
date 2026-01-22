package main
import (
"flag"
"fmt"
"log"
"net/http"
"net/http/httputil"
"net/url"
"os"
"os/signal"
"syscall"
"time"
)
func main() {
var (
listenAddr = flag.String("listen", "0.0.0.0:11434", "Address to listen on (default: 0.0.0.0:11434)")
targetAddr = flag.String("target", "http:
)
flag.Parse()
target, err := url.Parse(*targetAddr)
if err != nil {
log.Fatalf("Invalid target URL: %v", err)
}
proxy := httputil.NewSingleHostReverseProxy(target)
originalDirector := proxy.Director
proxy.Director = func(req *http.Request) {
originalDirector(req)
log.Printf("Proxying %s %s → %s", req.Method, req.URL.Path, target)
}
proxy.ErrorHandler = func(w http.ResponseWriter, r *http.Request, err error) {
log.Printf("Proxy error for %s %s: %v", r.Method, r.URL.Path, err)
w.WriteHeader(http.StatusBadGateway)
fmt.Fprintf(w, "Proxy error: %v", err)
}
server := &http.Server{
Addr:         *listenAddr,
Handler:      proxy,
ReadTimeout:  300 * time.Second,
WriteTimeout: 300 * time.Second,
}
c := make(chan os.Signal, 1)
signal.Notify(c, os.Interrupt, syscall.SIGTERM)
go func() {
<-c
log.Println("Shutting down proxy...")
server.Close()
}()
log.Printf("Starting universal proxy: %s → %s", *listenAddr, *targetAddr)
log.Printf("All traffic to any interface on port 11434 will be forwarded to echollama")
log.Printf("Test with: curl http:
if err := server.ListenAndServe(); err != http.ErrServerClosed {
log.Fatalf("Server failed: %v", err)
}
log.Println("Proxy stopped")
}