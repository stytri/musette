# Calculate squares

square = x => x x;

(a = 1; a <= 10) ?* {
	b = a, a = a + 1;
    (b == 5) ? (
        <:"Missing "b"!";
		?.;
    );
	<:"I can compute that "b"*"b" = "{square b};
};
