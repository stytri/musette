# Logic operations using user defined named operators

not=a=>a==0;
and=(a,b)=>(a!=0)*(b!=0);
or=(a,b)=>((a!=0)+(b!=0))!=0;

<:"not 0 = "{not 0};
<:"not 1 = "{not 1};
<:"not 8 = "{not 8};

<:"0 and 0 = "{0`and`0};
<:"0 and 1 = "{0`and`1};
<:"1 and 0 = "{1`and`0};
<:"1 and 1 = "{1`and`1};
<:"1 and 8 = "{1`and`8};
<:"8 and 1 = "{8`and`1};

<:"0 or 0 = "{0`or`0};
<:"0 or 1 = "{0`or`1};
<:"1 or 0 = "{1`or`0};
<:"1 or 1 = "{1`or`1};
<:"1 or 8 = "{1`or`8};
<:"8 or 1 = "{8`or`1};
