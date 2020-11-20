import matplotlib.pyplot as plt  # type: ignore
import pandas as pd  # type: ignore
import seaborn as sns  # type: ignore
from lingpy import LexStat  # type: ignore
from lingpy.evaluate.acd import bcubes  # type: ignore

sns.set(style="darkgrid", font_scale=2)


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
    records = []
    f_names = ["SLV", "GER", "ROM", "OUG", "KSL", "BAI", "JAP", "PIE", "IEL", "PAN"]
    for name in f_names:
        print(name)
        for n in range(1, 7):
            results = []
            for i in [50]:
                lex = LexStat("../{}_updating_{}_{}.csv".format(name, i, n))
                # lex.get_scorer()
                # lex.cluster(method="lexstat", threshold=0.6, ref="cognates")

                precision, recall, f_score = bcubes(
                    lex,
                    "cogid",
                    "newcogid",
                    pprint=False,
                    modify_ref=lambda x: abs(int(x)),
                )
                records.append([name, n, precision, recall, f_score])

            for r in results:
                print(r)
        print()

    f, axes = plt.subplots(1, 2, figsize=(20, 8))

    # markers = {i: "${}$".format(i) for i in range(1, 7)}
    df = pd.DataFrame.from_records(
        records, columns=["Partition", "Iteration", "Precision", "Recall", "F-score"]
    )
    sns.lineplot(
        x="Recall", y="Precision", hue="Partition", data=df, marker="o", ax=axes[0]
    )
    # plt.subplots_adjust(right=0.7)
    # plt.legend(bbox_to_anchor=(1.02, 1.02), loc="upper left")

    for _, i, precision, recall, _ in records:
        if i == 1 or i == 6:
            axes[0].annotate(str(i), (recall, precision))

    # markers = {i: "${}$".format(i) for i in range(1, 7)}
    df = pd.DataFrame.from_records(
        records, columns=["Partition", "Iteration", "Precision", "Recall", "F-score"]
    )
    sns.lineplot(x="Iteration", y="F-score", hue="Partition", data=df, ax=axes[1])
    # plt.subplots_adjust(right=0.7)
    axes[0].get_legend().remove()
    lgd = plt.legend(bbox_to_anchor=(1.02, 1.00), loc="upper left")

    plt.savefig("bbb.png", bbox_extra_artists=[lgd], bbox_inches="tight")


# Best 0.833
if __name__ == "__main__":
    main()
