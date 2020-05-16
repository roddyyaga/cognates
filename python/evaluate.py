from lingpy import LexStat  # type: ignore
from lingpy.evaluate.acd import bcubes  # type: ignore


def main():
    results = []
    for i in range(40):
        lex = LexStat("../PIE_scored_{}_og.csv".format(i))
        # lex.get_scorer()
        # lex.cluster(method="lexstat", threshold=0.6, ref="cognates")
        print(".", end="", flush=True)
        results.append(
            (
                i,
                bcubes(
                    lex,
                    "cogid",
                    "newcogid",
                    pprint=False,
                    modify_ref=lambda x: abs(int(x)),
                ),
            )
        )

    print()
    print("OG")
    for r in results:
        print(r)

    for n in range(1, 5):
        results = []
        for i in range(40):
            lex = LexStat("../PIE_scored_{}_{}.csv".format(i, n))
            # lex.get_scorer()
            # lex.cluster(method="lexstat", threshold=0.6, ref="cognates")
            print(".", end="", flush=True)
            results.append(
                (
                    i,
                    bcubes(
                        lex,
                        "cogid",
                        "newcogid",
                        pprint=False,
                        modify_ref=lambda x: abs(int(x)),
                    ),
                )
            )

        print()
        print("Nu", n)
        for r in results:
            print(r)


# Best 0.833
if __name__ == "__main__":
    main()
