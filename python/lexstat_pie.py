from lingpy import LexStat  # type: ignore
from lingpy.evaluate.acd import bcubes  # type: ignore


def main():
    for threshold in [0.2, 0.4, 0.6, 0.8]:
        lex = LexStat("../PIE_bognates.csv")
        lex.get_scorer()
        lex.cluster(method="lexstat", threshold=threshold, ref="cognates")
        bcubes(lex, "cogid", "cognates")


if __name__ == "__main__":
    main()
