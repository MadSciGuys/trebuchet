#!/bin/sh
#cd /home/mswan5/Trebuchet && git pull && cabal install --only-dependencies || exit 1
#LD_LIBRARY_PATH=/opt/rh/postgresql92/root/usr/lib64 \
exec cabal run -- \
  --oa-cookie-domain pku.pdms.jnj.com \
  --oa-host 10.37.49.14 \
  --oa-port 3306 \
  --oa-database "atrium" \
  --oa-username "atrium_app_pku" \
  --oa-password "!by8ktk+L\$4B" \
  --pg-host 10.37.49.94 \
  --pg-port 5432 \
  --pg-database trebuchet \
  --pg-username trebuchet \
  --pg-password trebuchet \
  --pg-pool-max 6 \
  --pg-conn-lifetime 30 \
  --port 8080 \
  #--ssl-certificate /home/mswan5/api.pku.cer \
  #--ssl-certificate-key /home/mswan5/api.pku.key \
  --job-template-directory job_templates
