from __future__ import annotations

import json
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Literal

import pandas as pd
from dotenv import load_dotenv
from langchain_core.output_parsers import PydanticOutputParser
from langchain_core.prompts import ChatPromptTemplate
from langchain_openai import ChatOpenAI
from pydantic import BaseModel, Field

load_dotenv()


class FilterCondition(BaseModel):
    column: str = Field(..., description="Column to filter in adae.csv")
    operator: Literal["equals", "contains", "not_equals", "not_null"] = Field(
        ..., description="Filter operator"
    )
    value: str = Field("", description="Value to compare against (if applicable)")


class LLMFilterResult(BaseModel):
    target_column: str = Field(
        ...,
        description=(
            "Primary dataset column to filter (e.g., AESEV for severity/intensity, "
            "AETERM for condition/term, AESOC for body system)."
        ),
    )
    filter_value: str = Field(
        ...,
        description=(
            "Primary value to search for in target_column, extracted from the question "
            "(e.g., MODERATE, HEADACHE, CARDIAC)."
        ),
    )
    target_operator: Literal["equals", "contains"] = Field(
        "equals", description="How to apply the primary filter"
    )
    additional_filters: List[FilterCondition] = Field(
        default_factory=list,
        description=(
            "Optional additional filter conditions (e.g., TRTEMFL == 'Y', AEREL != 'NONE')."
        ),
    )
    needs_clarification: bool = Field(
        False, description="True if the question is ambiguous or missing details"
    )
    clarification_question: str = Field(
        "", description="A short question asking the user to clarify"
    )


@dataclass
class QueryResult:
    filters: List[FilterCondition]
    count: int
    usubjids: List[str]


@dataclass
class ClarificationResult:
    clarification_question: str


class ClinicalTrialDataAgent:
    def __init__(self, df: pd.DataFrame, schema: Dict[str, str] | None = None) -> None:
        self.df = df
        self.schema = schema or {
            "AESEV": "Severity of adverse event (e.g., MILD, MODERATE, SEVERE)",
            "AETERM": "Adverse event term/condition (e.g., HEADACHE, COUGH)",
            "AESOC": "Body system category (e.g., CARDIAC DISORDERS)",
            "AEBODSYS": "Body system (similar to AESOC)",
            "AEDECOD": "Standardized AE term",
            "TRTEMFL": "Treatment-emergent flag (Y/N)",
            "AEREL": "Relationship to drug (NONE/REMOTE/POSSIBLE/PROBABLE)",
            "USUBJID": "Unique subject identifier",
        }

        self.parser = PydanticOutputParser(pydantic_object=LLMFilterResult)
        self.prompt = ChatPromptTemplate.from_messages(
            [
                (
                    "system",
                    "You are a clinical data assistant. Map the user question to the "
                    "most appropriate column and value from the schema. "
                    "If the question is ambiguous or missing details, set "
                    "needs_clarification=true and provide a short "
                    "clarification_question. Return only JSON that matches the schema. "
                    "Your JSON MUST include: target_column and filter_value.",
                ),
                (
                    "user",
                    "Schema:\n{schema}\n\nQuestion:\n{question}\n\n{format_instructions}",
                ),
            ]
        )

        self.llm = self._build_llm()

    def _build_llm(self):
        api_key = os.getenv("OPENAI_API_KEY")
        if not api_key:
            return None
        return ChatOpenAI(model="gpt-4o-mini", temperature=0)

    def _mock_llm_response(self, question: str) -> str:
        text = question.lower()
        target_column = ""
        filter_value = ""
        target_operator: str = "equals"
        additional_filters: List[dict] = []

        if "severe" in text:
            target_column, filter_value, target_operator = "AESEV", "SEVERE", "equals"
        if "moderate" in text:
            target_column, filter_value, target_operator = "AESEV", "MODERATE", "equals"
        if "mild" in text:
            target_column, filter_value, target_operator = "AESEV", "MILD", "equals"
        if "headache" in text:
            target_column, filter_value, target_operator = "AETERM", "HEADACHE", "equals"
        if "cardiac" in text:
            target_column, filter_value, target_operator = "AESOC", "CARDIAC", "contains"
        if "treatment-emergent" in text or "teae" in text:
            additional_filters.append(
                {"column": "TRTEMFL", "operator": "equals", "value": "Y"}
            )
        if "related to drug" in text or "drug-related" in text:
            additional_filters.append(
                {"column": "AEREL", "operator": "not_null", "value": ""}
            )
            additional_filters.append(
                {"column": "AEREL", "operator": "not_equals", "value": "NONE"}
            )

        if len(text.split()) < 3 or not target_column or not filter_value:
            result = {
                "target_column": "",
                "filter_value": "",
                "target_operator": "equals",
                "additional_filters": [],
                "needs_clarification": True,
                "clarification_question": (
                    "Can you clarify what you want to filter by "
                    "(e.g., severity, AE term, body system, "
                    "treatment-emergent, drug-related)?"
                ),
            }
        else:
            result = {
                "target_column": target_column,
                "filter_value": filter_value,
                "target_operator": target_operator,
                "additional_filters": additional_filters,
                "needs_clarification": False,
                "clarification_question": "",
            }
        return json.dumps(result)

    def parse_question(self, question: str) -> LLMFilterResult:
        schema_text = "\n".join([f"{k}: {v}" for k, v in self.schema.items()])
        if self.llm is None:
            raw = self._mock_llm_response(question)
            return self.parser.parse(raw)

        chain = self.prompt | self.llm | self.parser
        return chain.invoke(
            {
                "schema": schema_text,
                "question": question,
                "format_instructions": self.parser.get_format_instructions(),
            }
        )

    def execute(self, question: str) -> QueryResult | ClarificationResult:
        parsed = self.parse_question(question)
        if parsed.needs_clarification or not parsed.target_column or not parsed.filter_value:
            return ClarificationResult(
                clarification_question=parsed.clarification_question
                or "Could you clarify your question?"
            )

        filters: List[FilterCondition] = [
            FilterCondition(
                column=parsed.target_column,
                operator=parsed.target_operator,
                value=parsed.filter_value,
            )
        ] + list(parsed.additional_filters)

        mask = pd.Series([True] * len(self.df))

        for condition in filters:
            if condition.column not in self.df.columns:
                raise ValueError(f"Unknown column: {condition.column}")

            series = self.df[condition.column]
            value = condition.value

            if condition.operator == "not_null":
                mask &= series.notna() & series.astype(str).str.strip().ne("")
            elif condition.operator == "equals":
                mask &= (
                    series.fillna("").astype(str).str.upper()
                    == str(value).upper()
                )
            elif condition.operator == "not_equals":
                mask &= (
                    series.fillna("").astype(str).str.upper()
                    != str(value).upper()
                )
            elif condition.operator == "contains":
                mask &= series.fillna("").astype(str).str.contains(
                    str(value), case=False, regex=False
                )
            else:
                raise ValueError(f"Unsupported operator: {condition.operator}")

        filtered = self.df[mask]
        usubjids = sorted(filtered["USUBJID"].dropna().unique().tolist())

        return QueryResult(
            filters=filters,
            count=len(usubjids),
            usubjids=usubjids,
        )


def _default_data_path() -> Path:
    return (
        Path(__file__).resolve().parents[1]
        / "question_5_api"
        / "data"
        / "adae.csv"
    )


def run_cli() -> None:
    data_path = Path(os.getenv("ADAE_CSV_PATH", _default_data_path()))
    df = pd.read_csv(data_path, dtype=str)
    agent = ClinicalTrialDataAgent(df)

    divider = "-" * 60
    print(divider)
    print("ClinicalTrialDataAgent CLI")
    print("Type a question, or 'exit' to quit.")
    print(divider)

    while True:
        try:
            question = input("\nUser: ").strip()
        except EOFError:
            break

        if not question or question.lower() in {"exit", "quit"}:
            break

        try:
            result = agent.execute(question)
        except Exception as exc:
            print(f"Error: {exc}")
            continue

        if isinstance(result, ClarificationResult):
            print("Agent: I need a bit more detail.")
            print(f"Agent: {result.clarification_question}")
            print(divider)
            continue

        query_parts = []
        for condition in result.filters:
            if condition.operator == "not_null":
                query_parts.append(f"{condition.column} is not null")
            elif condition.operator == "equals":
                query_parts.append(f"{condition.column} == '{condition.value}'")
            elif condition.operator == "not_equals":
                query_parts.append(f"{condition.column} != '{condition.value}'")
            else:
                query_parts.append(
                    f"{condition.column} contains '{condition.value}'"
                )
        query_preview = " AND ".join(query_parts)

        print(f"Agent: Query -> {query_preview}")
        print(f"Agent: Result -> {result.count} subjects")
        print(f"Agent: USUBJIDs: {result.usubjids}")
        print(divider)


if __name__ == "__main__":
    run_cli()
