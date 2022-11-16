import React from "react";
import { useSliderState } from "react-stately";
import {
  mergeProps,
  useFocusRing,
  useNumberFormatter,
  useSlider,
  useSliderThumb,
  VisuallyHidden,
} from "react-aria";

function Thumb(props) {
  let { state, trackRef, index } = props;
  let inputRef = React.useRef(null);
  let { thumbProps, inputProps, isDragging } = useSliderThumb(
    {
      index,
      trackRef,
      inputRef,
    },
    state
  );

  let { focusProps, isFocusVisible } = useFocusRing();
  return (
    <div
      {...thumbProps}
      className={`thumb ${isFocusVisible ? "focus" : ""} ${
        isDragging ? "dragging" : ""
      }`}
    >
      <VisuallyHidden>
        <input ref={inputRef} {...mergeProps(inputProps, focusProps)} />
      </VisuallyHidden>
    </div>
  );
}

type Thumbs = {
  minThumb: number;
  maxThumb: number;
};
type Props = {
  label: string;
  maxValue: number;
  minValue: number;
  step: number;
  value: Thumbs;
  onChange: (value: Thumbs) => void;
};

export function rangeSlider_(props: Props) {
  let onChange = ([minThumb, maxThumb]: number[]) =>
    props.onChange({ minThumb, maxThumb });
  let value = [props.value.minThumb, props.value.maxThumb];

  let trackRef = React.useRef(null);

  let numberFormatter = useNumberFormatter({
    useGrouping: false,
    maximumFractionDigits: 0,
  });

  let state = useSliderState<number[]>({
    maxValue: props.maxValue,
    minValue: props.minValue,
    onChange,
    value,
    numberFormatter,
    label: props.label,
  });
  let { groupProps, trackProps, labelProps, outputProps } = useSlider(
    {
      value,
      maxValue: props.maxValue,
      minValue: props.minValue,
      onChange,
      label: props.label,
    },
    state,
    trackRef
  );

  return (
    <div {...groupProps} className={`slider ${state.orientation}`}>
      {props.label && (
        <div className="label-container">
          <label {...labelProps}>{props.label}</label>
          <output {...outputProps}>
            {`${state.getThumbValueLabel(0)} - ${state.getThumbValueLabel(1)}`}
          </output>
        </div>
      )}
      <div
        {...trackProps}
        ref={trackRef}
        className={`track ${state.isDisabled ? "disabled" : ""}`}
      >
        <Thumb index={0} state={state} trackRef={trackRef} />
        <Thumb index={1} state={state} trackRef={trackRef} />
      </div>
    </div>
  );
}
