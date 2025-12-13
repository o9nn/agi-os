#!/usr/bin/env python3
"""
Simple HTTP server for Deep Tree Echo dashboard
"""

import http.server
import socketserver
import os
from pathlib import Path

PORT = 8080
DIRECTORY = Path(__file__).parent

class CORSRequestHandler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=str(DIRECTORY), **kwargs)
    
    def end_headers(self):
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        super().end_headers()
    
    def do_OPTIONS(self):
        self.send_response(200)
        self.end_headers()

def main():
    with socketserver.TCPServer(("", PORT), CORSRequestHandler) as httpd:
        print(f"🌐 Deep Tree Echo Dashboard Server")
        print(f"=" * 50)
        print(f"Serving at: http://localhost:{PORT}")
        print(f"Dashboard: http://localhost:{PORT}/dashboard.html")
        print(f"=" * 50)
        print(f"Press Ctrl+C to stop")
        print()
        
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\n\n🛑 Server stopped")

if __name__ == "__main__":
    main()
