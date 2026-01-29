# Question 5: Clinical Data API (FastAPI)

## Setup
Install dependencies:
```
pip install -r requirements.txt
```

## Run
Start the API locally:
```
uvicorn main:app --reload
```

The API will be available at `http://127.0.0.1:8000`.

## Example requests
Welcome endpoint:
```
curl http://127.0.0.1:8000/
```

Dynamic AE filtering:
```
curl -X POST http://127.0.0.1:8000/ae-query \
  -H "Content-Type: application/json" \
  -d '{"severity":["MILD","MODERATE"],"treatment_arm":"Placebo"}'
```

Subject risk score:
```
curl http://127.0.0.1:8000/subject-risk/01-701-1015
```
