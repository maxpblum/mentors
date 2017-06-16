const asyncIterate = (iterate, initState) => {
  setTimeout(
    () => {
      const newValue = iterate(initState);
      asyncIterate(iterate, newValue);
    },
    0
  );
};
