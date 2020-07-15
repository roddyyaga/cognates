from lingpy import LexStat  # type: ignore


def main():
    for i, threshold in enumerate([-1, 0.0, 0.0001, 0.01, 0.1]):
        print(i)
        lex = LexStat("../data/ARM_GRE.csv")
        lex.get_scorer()
        lex.cluster(
            method="lexstat", threshold=threshold, ref="cognates", verbose=False
        )
        lex.output(
            "csv", filename="ARM_GRE_lexstat_{}".format(i), ignore="all", prettify=True
        )


if __name__ == "__main__":
    main()
