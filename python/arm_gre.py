from lingpy import LexStat  # type: ignore
from lingpy.evaluate.acd import bcubes  # type: ignore


def main():
    results = []
    """for i in range(20):
        lex = LexStat("../PIE_scored_{}_og.csv".format(i))
        # lex.get_scorer()
        # lex.cluster(method="lexstat", threshold=0.6, ref="cognates")
        print(".", end="", flush=True)
        results.append( (
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
:
    print()
    print("OG")
    for r in results:
        print(r)
"""
    for name in ["ARM_GRE"]:
        print(name)
        for n in range(1, 7):
            results = []
            for i in [50]:
                lex = LexStat("../{}_{}_{}.csv".format(name, i, n))
                # lex.get_scorer()
                # lex.cluster(method="lexstat", threshold=0.6, ref="cognates")
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

            for r in results:
                print(r)
        print()


# Best 0.833
if __name__ == "__main__":
    main()
