from lingpy import LexStat  # type: ignore
from lingpy.evaluate.acd import bcubes  # type: ignore


def main():
    results = []
    for name in ["ROM", "BAI", "GER", "JAP", "OUG", "PIE", "SLV", "IEL", "KSL", "PAN"]:
        print(name)
        for n in range(1, 7):
            results = []
            for i in [50]:
                lex = LexStat("../{}_updating_{}_{}.csv".format(name, i, n))
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
