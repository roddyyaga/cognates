from lingpy import LexStat  # type: ignore
from lingpy.evaluate.acd import bcubes  # type: ignore


def main():
    results = []
    for threshold in [0.5, 0.55, 0.6, 0.65]:
        lex = LexStat("../data/KSL.csv")
        lex.get_scorer()
        lex.cluster(
            method="lexstat", threshold=threshold, ref="cognates", verbose=False
        )
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
