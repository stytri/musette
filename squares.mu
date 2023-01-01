# Calculate squares

square = x => x x;

(a = 1; a <= 10) ?* {
	b = a, a = a + 1;
    (b == 5) ? (
        _<:"Missing "b"!";
		_?._;
    );
	_<:"I can compute that "b"*"b" = "{square b};
};
