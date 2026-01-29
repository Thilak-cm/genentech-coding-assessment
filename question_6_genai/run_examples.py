from __future__ import annotations

import os
from pathlib import Path

import pandas as pd

from agent import ClinicalTrialDataAgent, ClarificationResult

DEFAULT_DATA_PATH = (
    Path(__file__).resolve().parents[1]
    / "question_5_api"
    / "data"
    / "adae.csv"
)


def main() -> None:
    data_path = Path(os.getenv("ADAE_CSV_PATH", DEFAULT_DATA_PATH))
    df = pd.read_csv(data_path, dtype=str)

    agent = ClinicalTrialDataAgent(df)

    example_questions = [
        "Give me the subjects who had adverse events of Moderate severity",
        "Which subjects reported Headache?",
        "Which subjects experienced at least one SEVERE treatment-emergent adverse event related to drug?"
    ]

    for question in example_questions:
        result = agent.execute(question)
        print(f"\nQuestion: {question}")
        if isinstance(result, ClarificationResult):
            print(f"Clarify: {result.clarification_question}")
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
        print(f"Query -> {query_preview}")
        print(f"Result -> {result.count} subjects")
        print(f"USUBJIDs: {result.usubjids}")


if __name__ == "__main__":
    main()
