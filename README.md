# Tola Backend API


## Setup:

Prerequisite:
You need a PostgreSQL database

1. Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
2. Build: `stack build`
3. Run tests: `stack test`
4. Run app: `db="host=localhost dbname=test" port=3000 stack exec tola-backend-api-exe`

Run the test first, because it automatically creates the necessary SQL tables (`msisdn_submissions` and `pin_submissions`)

## Usage:

```bash
curl "http://localhost:3001/submit_msisdn/m.mobiworld.biz/gr/antivirus-kspr/1/?msisdn=6972865341"

curl "http://localhost:3000/submit_pin/?sid=10&pin=9196"
```

