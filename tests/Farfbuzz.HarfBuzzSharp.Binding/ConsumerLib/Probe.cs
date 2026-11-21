using HarfBuzzSharp;

namespace ConsumerLib
{
	public static class Probe
	{
		public static string Run () => Tag.Parse ("liga").ToString () + "|" + typeof (Tag).Assembly.FullName;
	}
}
