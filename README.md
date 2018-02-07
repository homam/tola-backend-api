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


----

`/tola/lodgement_notification`
```
{
   "accountname" : "...",
   "amount" : "40.0",
   "amounttype" : "unit",
   "channel" : "KENYA.SAFARICOM",
   "currency" : "KES",
   "customerreference" : "40",
   "date" : "2018-02-06T03:56:36Z",
   "mac" : "BCD6FABC38C05910AFD155935269FA30",
   "msisdn" : "...",
   "operatorreference" : "MB64LBJJZO",
   "reference" : "1.103.1517889396.4",
   "sourcereference" : "",
   "target" : "850702",
   "type" : "lodgement"
}
```

----

`/tola/disbursement_notification`
```
{
   "accountname" : "...",
   "amount" : "10.0",
   "amounttype" : "unit",
   "channel" : "KENYA.SAFARICOM",
   "currency" : "KES",
   "customerreference" : "dce2dddfffe.12",
   "date" : "2018-02-06T12:01:33Z",
   "mac" : "E85677101D82692AA0AA8CDAFFD1C188",
   "msisdn" : "...",
   "operatorreference" : "MB62LL5H58",
   "sourcereference" : "2.101.1517918395.1",
   "success" : "true",
   "target" : "850702",
   "type" : "notification"
}
```
