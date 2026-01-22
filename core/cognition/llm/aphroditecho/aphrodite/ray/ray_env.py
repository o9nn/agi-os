import json
import os
from typing import Optional
import aphrodite.common.envs as envs
from loguru import logger
CONFIG_HOME = envs.APHRODITE_CONFIG_ROOT
RAY_NON_CARRY_OVER_ENV_VARS_FILE = os.path.join(CONFIG_HOME, 'ray_non_carry_over_env_vars.json')
try:
    if os.path.exists(RAY_NON_CARRY_OVER_ENV_VARS_FILE):
        with open(RAY_NON_CARRY_OVER_ENV_VARS_FILE) as f:
            RAY_NON_CARRY_OVER_ENV_VARS = set(json.load(f))
    else:
        RAY_NON_CARRY_OVER_ENV_VARS = set()
except json.JSONDecodeError:
    logger.warning('Failed to parse {}. Using an empty set for non-carry-over env vars.', RAY_NON_CARRY_OVER_ENV_VARS_FILE)
    RAY_NON_CARRY_OVER_ENV_VARS = set()
def get_env_vars_to_copy(exclude_vars: Optional[set[str]]=None, additional_vars: Optional[set[str]]=None, destination: Optional[str]=None) -> set[str]:
    exclude_vars = exclude_vars or set()
    additional_vars = additional_vars or set()
    env_vars_to_copy = {v for v in set(envs.environment_variables).union(additional_vars) if v not in exclude_vars and v not in RAY_NON_CARRY_OVER_ENV_VARS}
    to_destination = ' to ' + destination if destination is not None else ''
    logger.info('RAY_NON_CARRY_OVER_ENV_VARS from config: {}', RAY_NON_CARRY_OVER_ENV_VARS)
    logger.info('Copying the following environment variables{}: {}', to_destination, [v for v in env_vars_to_copy if v in os.environ])
    logger.info('If certain env vars should NOT be copied, add them to {} file', RAY_NON_CARRY_OVER_ENV_VARS_FILE)
    return env_vars_to_copy