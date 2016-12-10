#!/bin/bash
# copy this file to run.sh
# make it executable
# updat all values
# ./run.sh will start play and SBT
# you can then work in development mode with ~run.sh
#
# Do not commit run.sh on your Github repo
# Use environment variables for production on Clever-cloud

export APP_SECRET="PAUGISTHEBESTMEETUPINTHEWORLD<3<3<3NOWLETSGOTO4NDR01DM4K<3<3<3RS"
export CFP_LANG="fr,fr-FR,en,en-US"
export CFP_HOSTNAME="cfp.androidmakers.fr"

export SMTP_HOST="in-v3.mailjet.com"
export SMTP_USER="c52d04ff3e3ee730068939e28c3241f7"
export SMTP_PASSWORD="f20f4b76d79011996bb6e1772fe86d85"
export SMTP_MOCK="false"
export SMTP_SSL="no"
export SMTP_PORT="587"

export MAIL_BCC="cfp@androidmakers.fr"
export MAIL_COMMITTEE="cfp@androidmakers.fr"
export MAIL_FROM="cfp@androidmakers.fr"
export MAIL_BUG_REPORT="cfp@androidmakers.fr"

export LINKEDIN_CLIEND_ID="TODO"
export LINKEDIN_SECRET="TODO"

export GITHUB_ID="0ff2e266cf9558e2110f"
export GITHUB_SECRET="11f78b4db9e7f3760dccc36906d7bf2aa3839326"

export GOOGLE_ID="495059484507-iqa42hivj16jd053dt02ev69478s5csj.apps.googleusercontent.com"
export GOOGLE_SECRET="o3OqTtfD1GuN8HC4lzxO8YwC"

export REDIS_HOST="bisj4w7o4-redis.services.clever-cloud.com"
export REDIS_PORT=3026
export REDIS_PASSWORD="vv62VMVEemd34yPxJGt"

export CRON_UPDATER="false"
export CRON_DAYS="2"

export ES_HOST="http://TODO:9200"
export ES_USERNAME="TODO"
export ES_PASSWORD="TODO"

export BITBUCKET_USERNAME="TODO"
export BITBUCKET_PASSWORD="TODO"
export BITBUCKET_URL="https://bitbucket.org/api/1.0/repositories/TODO/TODO/issues"

export OPSGENIE_API="TODO"
export OPSGENIE_NAME="TODO"

export CFP_IS_OPEN="false"

export ACTIVATE_GOLDEN_TICKET="false"

export SBT_SCALA_VERSION="2.10.4"

echo -n "--- Configured for development ---"

play
