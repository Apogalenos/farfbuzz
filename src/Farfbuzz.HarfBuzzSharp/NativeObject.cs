#nullable disable

using System;
using System.Runtime.InteropServices;

namespace HarfBuzzSharp
{
	/// <summary>Represents a native object.</summary>
	/// <remarks />
	public class NativeObject : IDisposable
	{
		private bool isDisposed;
		private GCHandle token;

		internal NativeObject ()
		{
			token = GCHandle.Alloc (this, GCHandleType.Weak);
			Handle = GCHandle.ToIntPtr (token);
		}

		/// <summary>Allows an object to try to free resources and perform other cleanup operations before it is reclaimed by garbage collection.</summary>
		/// <remarks />
		~NativeObject ()
		{
			Dispose (false);
		}

		/// <summary>Gets or sets the handle to the underlying native object.</summary>
		/// <value>The handle to the underlying native object.</value>
		/// <remarks />
		public virtual IntPtr Handle { get; protected set; }

		/// <summary>Releases the unmanaged resources used by the <see cref="T:HarfBuzzSharp.NativeObject" /> and optionally releases the managed resources.</summary>
		/// <param name="disposing"><see langword="true" /> to release both managed and unmanaged resources; <see langword="false" /> to release only unmanaged resources.</param>
		/// <remarks>Always dispose the object before you release your last reference to the <see cref="T:HarfBuzzSharp.NativeObject" />. Otherwise, the resources it is using will not be freed until the garbage collector calls the finalizer.</remarks>
		protected virtual void Dispose (bool disposing)
		{
			if (isDisposed) {
				return;
			}

			isDisposed = true;

			if (disposing) {
				DisposeHandler ();
				Handle = IntPtr.Zero;
			}

			if (token.IsAllocated) {
				token.Free ();
			}
		}

		/// <summary>Releases the unmanaged resources used.</summary>
		/// <remarks />
		protected virtual void DisposeHandler ()
		{
		}

		/// <summary>Releases all resources used by this <see cref="T:HarfBuzzSharp.NativeObject" />.</summary>
		/// <remarks>Always dispose the object before you release your last reference to the <see cref="T:HarfBuzzSharp.NativeObject" />. Otherwise, the resources it is using will not be freed until the garbage collector calls the finalizer.</remarks>
		public void Dispose ()
		{
			Dispose (true);
			GC.SuppressFinalize (this);
		}
	}
}
