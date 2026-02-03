#!/usr/bin/env python3
"""
Test script for the Time Crystal Daemon.

This script tests the daemon's IDL commands without the LLM interface.
"""

import json
import socket
import sys
import time


def send_command(sock_path: str, method: str, params: dict = None) -> dict:
    """Send a command to the daemon and return the response."""
    request = {
        'jsonrpc': '2.0',
        'id': 1,
        'method': method,
        'params': params or {}
    }

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        sock.connect(sock_path)
        sock.sendall(json.dumps(request).encode('utf-8') + b'\n')
        
        response = b''
        while True:
            chunk = sock.recv(4096)
            if not chunk:
                break
            response += chunk
            if b'\n' in response:
                break
        
        return json.loads(response.decode('utf-8'))
    finally:
        sock.close()


def test_daemon(sock_path: str = "/tmp/tc_daemon.sock"):
    """Run tests against the daemon."""
    print("=" * 60)
    print("Time Crystal Daemon Test Suite")
    print("=" * 60)
    print()

    tests = [
        ("get_status", {}),
        ("list_modules", {}),
        ("get_module", {"module_id": "pln"}),
        ("diagnose", {"scope": "all"}),
        ("get_tc_hierarchy", {}),
    ]

    passed = 0
    failed = 0

    for method, params in tests:
        print(f"Testing: {method}")
        try:
            result = send_command(sock_path, method, params)
            if 'error' in result:
                print(f"  ❌ Error: {result['error']}")
                failed += 1
            else:
                print(f"  ✓ Success")
                print(f"    Result: {json.dumps(result.get('result', {}), indent=2)[:200]}...")
                passed += 1
        except FileNotFoundError:
            print(f"  ❌ Daemon not running at {sock_path}")
            failed += 1
        except Exception as e:
            print(f"  ❌ Exception: {e}")
            failed += 1
        print()

    print("=" * 60)
    print(f"Results: {passed} passed, {failed} failed")
    print("=" * 60)

    return failed == 0


if __name__ == "__main__":
    sock_path = sys.argv[1] if len(sys.argv) > 1 else "/tmp/tc_daemon.sock"
    success = test_daemon(sock_path)
    sys.exit(0 if success else 1)
