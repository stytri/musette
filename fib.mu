# Calculate fibonacci numbers

fib = x => {
    (x < 2) ? _= x;
	fib(x-1) + fib(x-2)
};

(x = 0; x <= 20) ?* (
	_<:"fib "x" = "{fib x};
	x = x + 1
);
