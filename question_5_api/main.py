from __future__ import annotations

from pathlib import Path
from typing import List, Optional

import pandas as pd
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel

DATA_PATH = Path(__file__).parent / "data" / "adae.csv"

app = FastAPI(title="Clinical Trial Data API")


class AEQueryRequest(BaseModel):
    severity: Optional[List[str]] = None
    treatment_arm: Optional[str] = None


def load_ae_data() -> pd.DataFrame:
    if not DATA_PATH.exists():
        raise FileNotFoundError(f"Missing data file: {DATA_PATH}")
    return pd.read_csv(DATA_PATH, dtype=str)


AE_DF = load_ae_data()


@app.get("/")
def root() -> dict:
    return {"message": "Clinical Trial Data API is running"}


@app.post("/ae-query")
def ae_query(payload: AEQueryRequest) -> dict:
    df = AE_DF

    if payload.severity:
        df = df[df["AESEV"].isin(payload.severity)]

    if payload.treatment_arm:
        df = df[df["ACTARM"] == payload.treatment_arm]

    unique_subjects = sorted(df["USUBJID"].dropna().unique().tolist())
    return {"count": int(len(df)), "usubjids": unique_subjects}


@app.get("/subject-risk/{subject_id}")
def subject_risk(subject_id: str) -> dict:
    subject_df = AE_DF[AE_DF["USUBJID"] == subject_id]
    if subject_df.empty:
        raise HTTPException(status_code=404, detail="Subject not found")

    severity_weights = {"MILD": 1, "MODERATE": 3, "SEVERE": 5}
    risk_score = int(subject_df["AESEV"].map(severity_weights).fillna(0).sum())

    if risk_score < 5:
        risk_category = "Low"
    elif risk_score < 15:
        risk_category = "Medium"
    else:
        risk_category = "High"

    return {
        "subject_id": subject_id,
        "risk_score": risk_score,
        "risk_category": risk_category,
    }
