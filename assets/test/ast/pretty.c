int (f(int x, int y))
{
	{
		struct S
		{
			int x;
		} s;
		((s.x) = x);
		struct S (*p);
		(p = (&s));
		if (1)
			return (-((*(&(s.x))) = ((sizeof (s.x)) * ((p->x) - y))));
		else if (1)
			return 1;
		else {
			return 0;
		}
	}
}

void (*((*(*p))(int)));

int (main(void))
{
	int a;
	int b;
	{
		(a = 1);
	}
	(b = (a * 2));
	if ((a < b)) {
		while (((a < (sizeof b)) || (!(b == (f(23, (sizeof(int))))))))
foo:
			break;
	} else if (0) {
		char (*c);
		while (1) {
			(c = "bla");
			continue;
		}
	} else
		goto end;
bar:
end:
	return 0;
}
