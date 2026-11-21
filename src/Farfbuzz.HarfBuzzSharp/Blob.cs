#nullable disable

using System;
using System.IO;
using Farfbuzz.HarfBuzzSharp.Bridge;

namespace HarfBuzzSharp
{
	/// <summary>Represents a blob of data in memory.</summary>
	/// <remarks />
	public class Blob : NativeObject
	{
		private static readonly Lazy<Blob> emptyBlob = new Lazy<Blob> (() => new StaticBlob ());

		private readonly byte[] data;
		private ReleaseDelegate release;
		private bool immutable;

		/// <summary>Gets a reference to the empty <see cref="T:HarfBuzzSharp.Blob" /> instance.</summary>
		/// <value>The empty <see cref="T:HarfBuzzSharp.Blob" /> instance.</value>
		/// <remarks />
		public static Blob Empty => emptyBlob.Value;

		internal Blob (byte[] data, ReleaseDelegate release)
		{
			this.data = data;
			this.release = release;
		}

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.Blob" /> class, wrapping the specified data.</summary>
		/// <param name="data">The data to wrap.</param>
		/// <param name="length">The length of the data being wrapped.</param>
		/// <param name="mode">The memory mode to use.</param>
		/// <remarks>If there was a problem creating the blob, or if the data length was zero, then an empty blob will be created.</remarks>
		public Blob (IntPtr data, int length, MemoryMode mode)
			: this (data, length, mode, null)
		{
		}

		/// <summary>Initializes a new instance of the <see cref="T:HarfBuzzSharp.Blob" /> class, wrapping the specified data.</summary>
		/// <param name="data">The data to wrap.</param>
		/// <param name="length">The length of the data being wrapped.</param>
		/// <param name="mode">The memory mode to use.</param>
		/// <param name="releaseDelegate">The delegate to invoke when the data is not needed anymore.</param>
		/// <remarks>If there was a problem creating the blob, or if the data length was zero, then an empty blob will be created.</remarks>
		public unsafe Blob (IntPtr data, int length, MemoryMode mode, ReleaseDelegate releaseDelegate)
			: this (CopyOf (data, length, releaseDelegate), releaseDelegate)
		{
			if (this.data.Length == 0) {
				release = null;
			}
		}

		/// <summary>Releases the unmanaged resources used by the <see cref="T:HarfBuzzSharp.Blob" /> and optionally releases the managed resources.</summary>
		/// <param name="disposing"><see langword="true" /> to release both managed and unmanaged resources; <see langword="false" /> to release only unmanaged resources.</param>
		/// <remarks>Always dispose the object before you release your last reference to the <see cref="T:HarfBuzzSharp.Blob" />. Otherwise, the resources it is using will not be freed until the garbage collector calls the finalizer.</remarks>
		protected override void Dispose (bool disposing) =>
			base.Dispose (disposing);

		/// <summary>Releases the unmanaged resources used.</summary>
		/// <remarks />
		protected override void DisposeHandler ()
		{
			var pending = System.Threading.Interlocked.Exchange (ref release, null);
			pending?.Invoke ();
		}

		/// <summary>Gets the length of blob data in bytes.</summary>
		/// <value>The length of blob data in bytes.</value>
		/// <remarks />
		public int Length => data.Length;

		/// <summary>Gets the number of faces in this blob.</summary>
		/// <value>The number of faces in this blob.</value>
		/// <remarks />
		public int FaceCount => Faces.Count (data);

		/// <summary>Gets a value indicating whether the blob is immutable.</summary>
		/// <value><see langword="true" /> if the blob is immutable; otherwise, <see langword="false" />.</value>
		/// <remarks />
		public bool IsImmutable => immutable;

		/// <summary>Makes the blob immutable.</summary>
		/// <remarks />
		public void MakeImmutable ()
		{
			immutable = true;
		}

		/// <summary>Returns a stream that wraps the data.</summary>
		/// <returns>Returns the stream that wraps the data.</returns>
		/// <remarks>If the data is released, then the stream becomes invalid.</remarks>
		public Stream AsStream () =>
			new MemoryStream (data, writable: false);

		/// <summary>Returns a span that wraps the data.</summary>
		/// <returns>Returns the span that wraps the data.</returns>
		/// <remarks>If the data is released, then the span becomes invalid.</remarks>
		public Span<byte> AsSpan () =>
			new Span<byte> (data);

		/// <summary>Creates a new <see cref="T:HarfBuzzSharp.Blob" /> instance from the contents of the file.</summary>
		/// <param name="fileName">The path to the file to load.</param>
		/// <returns>Returns the new <see cref="T:HarfBuzzSharp.Blob" /> instance.</returns>
		/// <remarks />
		public static Blob FromFile (string fileName)
		{
			if (!File.Exists (fileName)) {
				throw new FileNotFoundException ("Unable to find file.", fileName);
			}

			return new Blob (File.ReadAllBytes (fileName), null);
		}

		/// <summary>Creates a new <see cref="T:HarfBuzzSharp.Blob" /> instance from the contents of the stream.</summary>
		/// <param name="stream">The stream to use.</param>
		/// <returns>Returns the new <see cref="T:HarfBuzzSharp.Blob" /> instance.</returns>
		/// <remarks />
		public static Blob FromStream (Stream stream)
		{
			using var ms = new MemoryStream ();
			stream.CopyTo (ms);
			return new Blob (ms.ToArray (), null);
		}

		internal ReadOnlyMemory<byte> Bytes => data;

		private static unsafe byte[] CopyOf (IntPtr data, int length, ReleaseDelegate releaseDelegate)
		{
			if (length <= 0 || data == IntPtr.Zero) {
				releaseDelegate?.Invoke ();
				return Array.Empty<byte> ();
			}

			return new ReadOnlySpan<byte> ((void*)data, length).ToArray ();
		}

		private class StaticBlob : Blob
		{
			public StaticBlob ()
				: base (Array.Empty<byte> (), null)
			{
			}

			protected override void Dispose (bool disposing)
			{
			}
		}
	}
}
