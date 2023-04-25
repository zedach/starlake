export SL_BIN_DIR="$(pwd)/../../distrib"
export SL_ENV=SNOWFLAKE
export SPARK_DRIVER_MEMORY=4G
export SL_FS=file://
export SL_ROOT="$(pwd)/quickstart"
export SL_AUDIT_SINK_TYPE=JdbcSink
export SL_METRICS_ACTIVE=true
export SL_ASSERTIONS_ACTIVE=true
export SL_SINK_TO_FILE=false
export SL_ANALYZE=false
export SL_HIVE=false
export SL_GROUPED=false
export SL_METRICS_PATH="/tmp/metrics/{domain}"
export SL_MAIN=ai.starlake.job.Main



if [[ -z "$AZURE_STORAGE_ACCOUNT" ]]; then
    echo "Must provide AZURE_STORAGE_ACCOUNT in environment"
    exit 1
fi

if [[ -z "$AZURE_STORAGE_CONTAINER" ]]; then
    echo "Must provide AZURE_STORAGE_CONTAINER in environment"
    exit 1
fi

if [[ -z "$AZURE_STORAGE_SAS" ]]; then
    echo "Must provide AZURE_STORAGE_SAS in environment"
    exit 1
fi

export SL_BIN_DIR="$PWD/../../distrib"
export SL_ENV=SNOWFLAKE
export SPARK_DRIVER_MEMORY=4G
export SL_FS="wasb://$AZURE_STORAGE_CONTAINER@$AZURE_STORAGE_ACCOUNT.blob.core.windows.net/"
export SL_ROOT="/mnt/starlake-app/quickstart"
export SL_METRICS_ACTIVE=true
export SL_ASSERTIONS_ACTIVE=true
export SL_AUDIT_SINK_TYPE=JdbcSink
export SL_SINK_TO_FILE=false
export SL_ANALYZE=false
export SL_HIVE=false
export SL_GROUPED=false
export SL_MAIN=ai.starlake.job.Main
export SL_METRICS_PATH="/mnt/starlake-app/quickstart/metrics/{domain}"
