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
    for name in ["PIE", "SLV", "OUG", "GER", "JAP", "ROM"]:
        print(name)
        for n in range(1, 8):
            results = []
            for i in range(1):
                lex = LexStat("../{}_one_{}_{}.csv".format(name, i, n))
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
        print()


# Best 0.833
if __name__ == "__main__":
    main()
