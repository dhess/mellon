## Mellon API


## GET /state

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`
    - `text/html;charset=utf-8`

- Locked

```javascript
{"state":"Locked","until":[]}
```

- Locked

```html
<!DOCTYPE HTML><html><head><title>Mellon state</title></head><body>Locked</body></html>
```

- Unlocked until a given date

```javascript
{"state":"Unlocked","until":"2015-10-06T00:00:00.000000000000Z"}
```

- Unlocked until a given date

```html
<!DOCTYPE HTML><html><head><title>Mellon state</title></head><body>Unlocked until 2015-10-06 00:00:00 UTC</body></html>
```

## PUT /state

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"state":"Locked","until":[]}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`
    - `text/html;charset=utf-8`

- Locked

```javascript
{"state":"Locked","until":[]}
```

- Locked

```html
<!DOCTYPE HTML><html><head><title>Mellon state</title></head><body>Locked</body></html>
```

- Unlocked until a given date

```javascript
{"state":"Unlocked","until":"2015-10-06T00:00:00.000000000000Z"}
```

- Unlocked until a given date

```html
<!DOCTYPE HTML><html><head><title>Mellon state</title></head><body>Unlocked until 2015-10-06 00:00:00 UTC</body></html>
```

## GET /time

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`
    - `text/html;charset=utf-8`

- 

```javascript
"2015-10-06T00:00:00.000000000000Z"
```

- 

```html
<!DOCTYPE HTML><html><head><title>Server time</title></head><body>Server time is 2015-10-06 00:00:00 UTC</body></html>
```

