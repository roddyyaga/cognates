from lingpy import LexStat  # type: ignore
from lingpy.evaluate.acd import bcubes  # type: ignore


def main():
    lex = LexStat("../PIE_bognates.csv")
    lex.get_scorer()
    lex.cluster(method="lexstat", threshold=0.6, ref="cognates")
    bcubes(lex, "cogid", "cognates")
    bcubes(lex, "cogid", "bognates")


if __name__ == "__main__":
    main()
