import socket
import sys
SOCKET_PATH = '/tmp/echo_socket'
def main():
    try:
        client_socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        try:
            client_socket.connect(SOCKET_PATH)
        except socket.error as e:
            print(f'Error connecting to server: {e}', file=sys.stderr)
            print('Make sure the server is running first.', file=sys.stderr)
            sys.exit(1)
        print('Connected to server. Type messages (Ctrl+D to exit):', file=sys.stderr)
        try:
            for line in sys.stdin:
                message = line.strip()
                if message:
                    client_socket.send(message.encode('utf-8'))
                    response = client_socket.recv(4096)
                    response = response.strip()
                    if response:
                        print(response.decode('utf-8'))
                        sys.stdout.flush()
                    else:
                        print('Server closed connection', file=sys.stderr)
                        break
        except KeyboardInterrupt:
            print('\nClient shutting down...', file=sys.stderr)
    except Exception as e:
        print(f'Client error: {e}', file=sys.stderr)
    finally:
        client_socket.close()
if __name__ == '__main__':
    main()