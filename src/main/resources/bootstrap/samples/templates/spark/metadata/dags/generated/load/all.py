# This template executes individual bash jobs and requires the following dag generation options set:
#
# - SL_ROOT: The root project path
# - SL_STARLAKE_PATH: the path to the starlake executable
#

import os
import re
from datetime import timedelta
from os import environ

# The DAG object; we'll need this to instantiate a DAG
from airflow import DAG

# Operators; we need this to operate!
from airflow.operators.bash import BashOperator
from airflow.utils.dates import days_ago
from airflow.utils.task_group import TaskGroup
from airflow.operators.dummy import DummyOperator
from airflow.models import Variable



description='sample dag configuration'
template='load/scheduled_table_bash.py.j2'

options = {
    'jarFileUrisVar':'SL_JARS', 
    'profileVar':'DATAPROC_MEDIUM', 
    'envVar':'SL_ENV', 
    'SL_TIMEZONE':'Europe/Paris', 
    'region':'europe-west1'
    
}

schedules= [
    {
        'schedule': 'None',
        'cron': None,
    'domains': [
    {
        'name':'hr',
        'tables': [
            {
                'name': 'sellers',
                'final_name': 'sellers'
            }
        ]
    },
    
    {
        'name':'sales',
        'tables': [
            {
                'name': 'customers',
                'final_name': 'customers'
            },
            {
                'name': 'orders',
                'final_name': 'orders'
            }
        ]
    }
    
]
},

    {
        'schedule': 'cron1',
        'cron': '0 0 * * *',
    'domains': [
    {
        'name':'hr',
        'tables': [
            {
                'name': 'locations',
                'final_name': 'locations'
            }
        ]
    }
    
]
}

]


class MissingEnvironmentVariable(Exception):
    pass

def get_context_var(var_name):
    if hasattr(options, var_name):
        return getattr(options, var_name)
    elif Variable.get(var_name) is not None:
        return Variable.get(var_name)
    elif os.getenv(var_name) is not None:
        return os.getenv(var_name)
    else:
        raise MissingEnvironmentVariable(f"{var_name} does not exist")


def generate_dag_name(schedule):
    base_dag_name = os.path.basename(__file__).replace(".py", "").replace(".pyc", "")
    return (f"{base_dag_name}-{schedule['schedule']}" if len(schedules) > 1 else base_dag_name)

def keep_ascii_only(text):
    return re.sub(r'[^\x00-\x7F]+', '_', text)


def sanitize_id(id: str):
    return keep_ascii_only(re.sub("[^a-zA-Z0-9\-_]", "_", id.replace("$", "S")))


default_args = {
    'owner': 'airflow',
    'depends_on_past': False,
    'email_on_failure': False,
    'email_on_retry': False,
    'retries': 1,
    'start_date': days_ago(2),
    'retry_delay': timedelta(minutes=5),
    #'email': ['airflow@example.com'],
    # 'queue': 'bash_queue',
    # 'pool': 'backfill',
    # 'priority_weight': 10,
    # 'end_date': datetime(2016, 1, 1),
    # 'wait_for_downstream': False,
    # 'dag': dag,
    # 'sla': timedelta(hours=2),
    # 'execution_timeout': timedelta(seconds=300),
    # 'on_failure_callback': some_function,
    # 'on_success_callback': some_other_function,
    # 'on_retry_callback': another_function,
    # 'sla_miss_callback': yet_another_function,
    # 'trigger_rule': 'all_success'
}


# [START instantiate_dag]
for schedule in schedules:
    with DAG(generate_dag_name(schedule),
             schedule_interval=schedule['cron'],
             default_args=default_args,
             catchup=False,
             description=description) as dag:
        start = DummyOperator(task_id="start")
        def generate_task_group_for_domain(domain):
            with TaskGroup(group_id=sanitize_id(f'{domain["name"]}_load_tasks')) as domain_load_tasks:
                for table in domain["tables"]:
                    load_task_id = sanitize_id(f'{domain["name"]}_{table["name"]}')
                    load_task = BashOperator(task_id=load_task_id,
                                             bash_command=get_context_var("SL_STARLAKE_PATH") + ' --domains ' + domain["name"] + ' --tables ' + table["name"] + ' load',
                                             cwd=get_context_var("SL_ROOT"),
                                             dag=dag)
                    load_task
            return domain_load_tasks

        all_load_tasks = [generate_task_group_for_domain(domain) for domain in schedule["domains"]]

        end = DummyOperator(task_id='end')
        start >> all_load_tasks >> end



