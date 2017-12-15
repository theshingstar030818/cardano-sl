curl -k --fail --request POST \
  --url https://127.0.0.1:8090/api/wallets/keys \
  --header 'content-type: application/json' \
  --data '"dev-keys/1.key"'

for i in `seq 1 15`;
do
  curl -k --fail --request GET \
  --url 'https://127.0.0.1:8090/api/settings/sync/progress'
  echo "\n+10000 addrs";
done
