import subprocess
import sys
import time
import os
TESTS_ROUNDS = 10
FAILED_TIME = -1.0
ANY_ROUND_FAILED = False
def set_dot_env_file():
    if os.path.exists('.env'):
        os.remove('.env')
    with open('.env', 'w') as f:
        f.write('DAS_MONGODB_HOSTNAME=localhost\nDAS_MONGODB_PORT=38000\nDAS_MONGODB_USERNAME=dbadmin\nDAS_MONGODB_PASSWORD=dassecret\nDAS_REDIS_HOSTNAME=localhost\nDAS_REDIS_PORT=39000\n')
def force_stop(pattern: str):
    if 'run-attention-broker' in pattern:
        pattern = 'attention_broker'
    elif 'run-query-agent' in pattern:
        pattern = 'query_broker'
    else:
        return
    subprocess.run("docker rm -f $(docker ps -a | awk '/" + pattern + "/ {print $1}')", shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT, check=False)
def start_process(command: str) -> subprocess.Popen:
    force_stop(command)
    return subprocess.Popen(command, shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT, preexec_fn=os.setsid)
def stop_process(process: subprocess.Popen):
    force_stop(str(process.args))
    os.killpg(os.getpgid(process.pid), subprocess.signal.SIGTERM)
    process.terminate()
    process.wait()
def run_command(command: str, check: bool=True) -> float:
    start_time = time.perf_counter()
    try:
        subprocess.run(command, shell=True, check=check, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError:
        return FAILED_TIME
    end_time = time.perf_counter()
    execution_time = end_time - start_time
    return execution_time
def main():
    set_dot_env_file()
    queries: dict[str, str] = dict(linktemplate_3_node_var_link='\n            LINK_TEMPLATE Expression 3\n                NODE Symbol Contains\n                VARIABLE sentence1\n                LINK Expression 2\n                    NODE Symbol Word\n                    NODE Symbol \'"aaa"\'\n        ', and_2_linktemplate_linktemplate='\n            AND 2\n                LINK_TEMPLATE Expression 3\n                    NODE Symbol Contains\n                    VARIABLE sentence1\n                    LINK Expression 2\n                        NODE Symbol Word\n                        NODE Symbol \'"bbb"\'\n                LINK_TEMPLATE Expression 3\n                    NODE Symbol Contains\n                    VARIABLE sentence2\n                    LINK Expression 2\n                        NODE Symbol Word\n                        NODE Symbol \'"aaa"\'\n        ', and_2_linktemplate_or_2_linktemplate_linktemplate='\n            AND 2\n                LINK_TEMPLATE Expression 3\n                    NODE Symbol Contains\n                    VARIABLE sentence1\n                    LINK Expression 2\n                        NODE Symbol Word\n                        NODE Symbol \'"bbb"\'\n                OR 2\n                    LINK_TEMPLATE Expression 3\n                        NODE Symbol Contains\n                        VARIABLE sentence2\n                        LINK Expression 2\n                            NODE Symbol Word\n                            NODE Symbol \'"aaa"\'\n                    LINK_TEMPLATE Expression 3\n                        NODE Symbol Contains\n                        VARIABLE sentence3\n                        LINK Expression 2\n                            NODE Symbol Word\n                            NODE Symbol \'"ccc"\'\n        ')
    cmd_prefix = "bash src/scripts/run.sh query 'localhost:31701' 'localhost:31700' false 1"
    cmd_suffix = ''
    print('Starting Attention Broker...', flush=True)
    attention_broker_process = start_process('make run-attention-broker')
    time.sleep(3)
    query_agent_process = start_process('make run-query-agent')
    time.sleep(3)
    stop_process(query_agent_process)
    time.sleep(3)
    for name, query in queries.items():
        print(f"\nRunning query '{name}'...")
        execution_time: float = 0.0
        print(f'Rounds [for round in range({TESTS_ROUNDS})]:', flush=True)
        valid_rounds = TESTS_ROUNDS
        for round in range(TESTS_ROUNDS):
            query_agent_process = start_process('make run-query-agent')
            time.sleep(3)
            print(f'  {round}: ', flush=True, end='')
            round_time = run_command(cmd_prefix + query.replace('\n', ' ') + cmd_suffix)
            stop_process(query_agent_process)
            if round_time != FAILED_TIME:
                execution_time += round_time
                print(f'{round_time:.2f} seconds')
            else:
                print('Failed')
                valid_rounds -= 1
                global ANY_ROUND_FAILED
                ANY_ROUND_FAILED = True
        execution_time_avg = execution_time / valid_rounds
        print(f"Average time for '{name}': {execution_time_avg:.2f} seconds (over {valid_rounds} rounds)")
    print('\nStopping Attention Broker...', flush=True)
    stop_process(attention_broker_process)
if __name__ == '__main__':
    main()
    sys.exit(-1 if ANY_ROUND_FAILED else 0)