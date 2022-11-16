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

type PropsF<X> = {
  value: X;
  onChange: (value: X) => void;
  fromForeign: (n: number) => (m: number) => X;
  toForeign: (x: X) => [number, number];
};

type Props = {
  label: string;
  maxValue: number;
  minValue: number;
  step: number;
  mkStateProps: <R>(run: <X>(run: PropsF<X>) => R) => R;
};

export function rangeSlider_({ mkStateProps, ...props }: Props) {
  return mkStateProps((stateProps) => {
    let onChange = ([n, m]: number[]) =>
      stateProps.onChange(stateProps.fromForeign(n)(m));
    let value = stateProps.toForeign(stateProps.value);

    let trackRef = React.useRef(null);

    let numberFormatter = useNumberFormatter();
    let state = useSliderState<number[]>({
      ...props,
      onChange,
      value,
      numberFormatter,
    });
    let { groupProps, trackProps, labelProps, outputProps } = useSlider(
      props,
      state,
      trackRef
    );

    return (
      <div {...groupProps} className={`slider ${state.orientation}`}>
        {props.label && (
          <div className="label-container">
            <label {...labelProps}>{props.label}</label>
            <output {...outputProps}>
              {`${state.getThumbValueLabel(0)} - ${state.getThumbValueLabel(
                1
              )}`}
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
  });
}
