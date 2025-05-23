import clsx from "clsx";
import Heading from "@theme/Heading";
import styles from "./styles.module.css";

type FeatureItem = {
  title: string;
  Svg: React.ComponentType<React.ComponentProps<"svg">>;
  description: JSX.Element;
};

const FeatureList: FeatureItem[] = [
  {
    title: "Intuitive State Monitoring",
    Svg: require("@site/static/img/undraw_term-sheet_70lo.svg").default,
    description: (
      <>
        Easily monitor the hydra head state through an intuitive and
        user-friendly api.
      </>
    ),
  },
  {
    title: "Control Your Hydra Head",
    Svg: require("@site/static/img/undraw_version-control_eiam.svg").default,
    description: (
      <>
        Effortlessly initialize your Hydra head, commit and de-commit your
        UTxOs, abort when necessary, and seamlessly close and fanout
        transactions to the mainchain.
      </>
    ),
  },
  {
    title: "Simplified Wallet Integration and Transaction Building",
    Svg: require("@site/static/img/undraw_online-banking_v7ih.svg").default,
    description: (
      <>
        Easily connect your wallet, create and submit valid hydra transactions
        with simple json architecture.
      </>
    ),
  },
];

function Feature({ title, Svg, description }: FeatureItem) {
  return (
    <div className={clsx("col col--4")}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures(): JSX.Element {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
