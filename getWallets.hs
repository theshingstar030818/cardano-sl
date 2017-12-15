while
/usr/bin/time --format "\n  %E\n" curl -k --request GET \
  --url https://127.0.0.1:8090/api/wallets --fail
do
echo "getWallets";
done
