import * as Plot from "@observablehq/plot";
import * as d3 from "d3";
import React from "react";

type Datum = {
  readonly year: number;
  readonly month: string;
  readonly value: number;
};

type Props = {
  readonly data: ReadonlyArray<Datum>;
  readonly width: number;
  readonly min: Date;
  readonly max: Date;
};

export function _largeChart(props: Props) {
  const ref = React.useRef();

  React.useEffect(() => {
    const chart = Plot.plot({
      width: props.width,
      style: {
        background: "transparent",
      },
      y: {
        label: "↑ ($)",
        grid: true,
      },
      marks: [
        Plot.areaY(
          props.data
            .map((d) => ({
              ...d,
              x: d3.timeParse("%B, %Y")(`${d.month}, ${d.year}`),
            }))
            .filter((d) => d.x >= props.min && d.x <= props.max),
          {
            x: (d: Datum & { readonly x: Date }) => d.x,
            y: (d: Datum) => d.value,
            fill: "tomato",
            opacity: 0.2,
          }
        ),
        Plot.lineY(
          props.data
            .map((d) => ({
              ...d,
              x: d3.timeParse("%B, %Y")(`${d.month}, ${d.year}`),
            }))
            .filter((d) => d.x >= props.min && d.x <= props.max),
          {
            x: (d: Datum & { readonly x: Date }) => d.x,
            y: (d: Datum) => d.value,
            strokeWidth: 1,
          }
        ),
      ],
    });
    ref.current.append(chart);
    return () => chart.remove();
  }, [props]);

  return <div ref={ref}></div>;
}
