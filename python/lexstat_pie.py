from lingpy import LexStat  # type: ignore
from lingpy.evaluate.acd import bcubes  # type: ignore


def main():
    results = []
    for threshold in [0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0]:
        lex = LexStat("../PIE_bognates.csv")
        lex.get_scorer()
        lex.cluster(method="lexstat", threshold=threshold, ref="cognates")
        results.append(
            (
                threshold,
                bcubes(lex, "cogid", "cognates", modify_ref=lambda x: abs(int(x))),
            )
        )

    for r in results:
        print(r)


if __name__ == "__main__":
    main()
