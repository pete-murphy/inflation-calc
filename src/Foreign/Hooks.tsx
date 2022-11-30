import useResizeObserver from "@react-hook/resize-observer";
import React from "react";

type Props = {
  readonly ref: React.MutableRef<HTMLElement>;
  readonly callback: (_: {
    readonly width: number;
    readonly height: number;
  }) => void;
};
export const _useResizeObserver = (props: Props) =>
  useResizeObserver(props.ref, (element) =>
    props.callback(element.contentRect)
  );
