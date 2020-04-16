from lingpy import LexStat  # type: ignore
from lingpy.evaluate.acd import bcubes  # type: ignore


def main():
    for i in range(20):
        lex = LexStat("../PIE_scored_{}.csv".format(i))
        # lex.get_scorer()
        # lex.cluster(method="lexstat", threshold=0.6, ref="cognates")
        print("Threshold", i)
        bcubes(lex, "cogid", "newcogid")


if __name__ == "__main__":
    main()
