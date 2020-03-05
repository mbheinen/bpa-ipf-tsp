main()
{
  double this;
  double that;
  double other;
  double plus;
  int i;
  
  this = 56650;
  plus = 0.01;

  that = 1.05;

  for(i=0; i<10; i++) {
    other = this * that;
    printf("%lf = %lf * %lf\n", other, this, that);
	that += plus;
  }
}
