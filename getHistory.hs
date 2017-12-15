while
/usr/bin/time --format "\n  %E\n" curl -k --request GET \
  --url 'https://127.0.0.1:8090/api/txs/histories?accountId=FHnt4NL7yPXtphUNYiKcgXe9vNvh1xFiJtPvLLCReTPnW8tVhL6RJviVN25k773@2147483648' --fail
do echo "\ngetHistory";
done
