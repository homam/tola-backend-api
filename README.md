# tola-backend-api

```bash
db="host=localhost dbname=tola" \
port=3001 \
tola_username="..." \
tola_password="..." \
tola_url="https://httpbin.org/post" \
tola_secret="..." \
mocked="true" \
stack exec tola-backend-api-march-exe
```

In the real server, remove `mocked="true"` env var.

---

## API

**Make a Charge Request**

```javascript
const msisdn = "254748103132"
const message = "PAY_NOW" // max 16 char
const price = 10
const version = "mock" // use mocked version for testing
fetch(`https://${root}/${version}/api/charge/${msisdn}/${price}/${message}`)
.then(x => x.json())
.then(x => console.log(x))
.catch(x => console.error(x))
```

Returns 

```json
{
  "tolaResponse": {"success":true,"reference":"5677316abcde0"},
  "chargeRequestId":"dd5a181b7df.1f"
}
```


**Check the State of a Charge Request**

```javascript
const chargeRequestId = "dd5a35892eb.46"
fetch(`https://${root}/${version}/api/check_charge/${chargeRequestId}`)
.then(x => x.json())
.then(x => console.log(x))
.catch(x => console.error(x))
```

Returns:

```json
{
  "state":"SuccessDisbursementNotificationReceived",
  "reference":"5677316abcde0",
  "errorMessage":null
}
```

Possible states:

* `SuccessChargeResponseReceived`
* `SuccessLodgementNotificationReceived`
* `SuccessDisbursementNotificationReceived`

### Full Example:

```javascript
const root = "..."
const version = "mock" // use mocked version for testing

function chargeAndWait(msisdn, message, price){

  const wait = ms => new Promise(res => setTimeout(res, ms))

  return fetch(`https://${root}/${version}/api/charge/${msisdn}/${price}/${message}`)
  .then(x => x.json())
  .then(x => {
      console.log(x)
      const chargeRequestId = x.chargeRequestId
      const check = (i) => i > 300 // 5 minutes
          ? Promise.reject({error: 'Timeout'}) 
          : fetch(`https://${root}/${version}/api/check_charge/${chargeRequestId}`)
            .then(x => x.json())
            .then(x => { 
              console.log(x)
              return x.state == 'SuccessDisbursementNotificationReceived'
              ? x
              : wait(1000).then(() => check(i+1))
            })
      return check(0)
  })

}

chargeAndWait("254748103132", "PAY_NOW", 10)
  .then(x => console.log('Charge Received ', x))
  .catch(console.error)
```

## Rockman Pixels

Run the executable with `--pixels` param:

```
db="..." \
port=3001 \
tola_username="..." \
tola_password="..." \
tola_url="https://api.ea.oxygen8.com/sammedia" \
tola_secret="..." \
stack exec tola-backend-api-exe -- --pixels
```