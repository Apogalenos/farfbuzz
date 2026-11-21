/*
 * Farfbuzz oracle: HarfBuzz compiled as one translation unit, plus extern "C"
 * shims exposing internals that the public API cannot reach, so that the
 * differential tests can compare Farfbuzz with HarfBuzz at that level too.
 *
 * Append-only: add shims at the end, never change or remove one.
 */

#include "harfbuzz.cc"

#if defined(_WIN32)
#define FARFBUZZ_ORACLE_EXPORT extern "C" __declspec(dllexport)
#else
#define FARFBUZZ_ORACLE_EXPORT extern "C" __attribute__((visibility("default")))
#endif

/* Prototypes first: HarfBuzz builds with -Werror=missing-prototypes. */
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_shims_version (void);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_indic_categories (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_use_category (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_arabic_joining_type (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_is_extended_pictographic (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_is_default_ignorable (hb_codepoint_t u);

/* Version of this shim set; bump when shims are appended. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_shims_version (void)
{
  return 1;
}

/* Indic category in the low byte, Indic position in the high byte (hb-ot-shaper-indic.hh). */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_indic_categories (hb_codepoint_t u)
{
  return hb_indic_get_categories (u);
}

/* Universal Shaping Engine category (hb-ot-shaper-use-table.hh). */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_use_category (hb_codepoint_t u)
{
  return hb_use_get_category (u);
}

/* Arabic joining type after the general-category resolution of JOINING_TYPE_X (hb-ot-shaper-arabic.cc). */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_arabic_joining_type (hb_codepoint_t u)
{
  return get_joining_type (u, hb_unicode_funcs_get_default ()->general_category (u));
}

/* Extended_Pictographic property (hb-unicode-emoji-table.hh). */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_is_extended_pictographic (hb_codepoint_t u)
{
  return _hb_unicode_is_emoji_Extended_Pictographic (u);
}

/* Default_Ignorable_Code_Point as HarfBuzz defines it (hb-unicode.hh). */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_is_default_ignorable (hb_codepoint_t u)
{
  return hb_unicode_funcs_t::is_default_ignorable (u);
}

FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_nominal_glyph_range (hb_font_t *font, hb_codepoint_t first, unsigned count, uint32_t *glyphs);

/* Nominal glyphs for a contiguous code-point range; writes 0xFFFFFFFF for misses and returns the count found. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_nominal_glyph_range (hb_font_t *font, hb_codepoint_t first, unsigned count, uint32_t *glyphs)
{
  unsigned found = 0;
  for (unsigned i = 0; i < count; i++)
  {
    hb_codepoint_t glyph = 0xFFFFFFFFu;
    if (hb_font_get_nominal_glyph (font, first + i, &glyph))
      found++;
    else
      glyph = 0xFFFFFFFFu;
    glyphs[i] = glyph;
  }
  return found;
}

FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_general_category (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_combining_class (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT hb_codepoint_t farfbuzz_oracle_mirroring (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT hb_codepoint_t farfbuzz_oracle_script (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_east_asian_width (hb_codepoint_t u);

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_general_category (hb_codepoint_t u)
{
  return hb_unicode_funcs_get_default ()->general_category (u);
}
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_combining_class (hb_codepoint_t u)
{
  return hb_unicode_funcs_get_default ()->combining_class (u);
}
FARFBUZZ_ORACLE_EXPORT hb_codepoint_t
farfbuzz_oracle_mirroring (hb_codepoint_t u)
{
  return hb_unicode_funcs_get_default ()->mirroring (u);
}
FARFBUZZ_ORACLE_EXPORT hb_codepoint_t
farfbuzz_oracle_script (hb_codepoint_t u)
{
  return hb_unicode_funcs_get_default ()->script (u);
}
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_east_asian_width (hb_codepoint_t u)
{
  (void) u;
  return 1;
}

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_compose (hb_codepoint_t a, hb_codepoint_t b, hb_codepoint_t *ab);
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_decompose (hb_codepoint_t ab, hb_codepoint_t *a, hb_codepoint_t *b);

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_compose (hb_codepoint_t a, hb_codepoint_t b, hb_codepoint_t *ab)
{
  return hb_unicode_funcs_get_default ()->compose (a, b, ab);
}
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_decompose (hb_codepoint_t ab, hb_codepoint_t *a, hb_codepoint_t *b)
{
  return hb_unicode_funcs_get_default ()->decompose (ab, a, b);
}

FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_modified_ccc (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_is_variation_selector (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_space_fallback (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT hb_codepoint_t farfbuzz_oracle_vertical_char (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_unicode_props (hb_codepoint_t u, unsigned *scratch);

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_modified_ccc (hb_codepoint_t u)
{
  return hb_unicode_funcs_get_default ()->modified_combining_class (u);
}
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_is_variation_selector (hb_codepoint_t u)
{
  return hb_unicode_funcs_t::is_variation_selector (u);
}
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_space_fallback (hb_codepoint_t u)
{
  return hb_unicode_funcs_t::space_fallback_type (u);
}
FARFBUZZ_ORACLE_EXPORT hb_codepoint_t
farfbuzz_oracle_vertical_char (hb_codepoint_t u)
{
  return hb_unicode_funcs_t::vertical_char_for (u);
}
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_unicode_props (hb_codepoint_t u, unsigned *scratch)
{
  hb_buffer_t *buffer = hb_buffer_create ();
  hb_glyph_info_t info = {};
  info.codepoint = u;
  _hb_glyph_info_set_unicode_props (&info, buffer);
  *scratch = buffer->scratch_flags;
  unsigned props = info.var2.u16[0];
  hb_buffer_destroy (buffer);
  return props;
}

FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_buffer_add_utf8 (hb_buffer_t *buffer, const uint8_t *text, int textLength, int itemOffset, int itemLength);
FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_buffer_add_utf16 (hb_buffer_t *buffer, const uint16_t *text, int textLength, int itemOffset, int itemLength);
FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_buffer_add_utf32 (hb_buffer_t *buffer, const uint32_t *text, int textLength, int itemOffset, int itemLength);
FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_buffer_add_latin1 (hb_buffer_t *buffer, const uint8_t *text, int textLength, int itemOffset, int itemLength);
FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_buffer_state (hb_buffer_t *buffer, uint32_t *codepoints, uint32_t *clusters, uint32_t *before, uint32_t *after, unsigned *length, unsigned *beforeLength, unsigned *afterLength, unsigned *content);

FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_buffer_add_utf8 (hb_buffer_t *buffer, const uint8_t *text, int textLength, int itemOffset, int itemLength)
{ hb_buffer_add_utf8 (buffer, (const char *) text, textLength, itemOffset, itemLength); }
FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_buffer_add_utf16 (hb_buffer_t *buffer, const uint16_t *text, int textLength, int itemOffset, int itemLength)
{ hb_buffer_add_utf16 (buffer, text, textLength, itemOffset, itemLength); }
FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_buffer_add_utf32 (hb_buffer_t *buffer, const uint32_t *text, int textLength, int itemOffset, int itemLength)
{ hb_buffer_add_utf32 (buffer, text, textLength, itemOffset, itemLength); }
FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_buffer_add_latin1 (hb_buffer_t *buffer, const uint8_t *text, int textLength, int itemOffset, int itemLength)
{ hb_buffer_add_latin1 (buffer, text, textLength, itemOffset, itemLength); }
FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_buffer_state (hb_buffer_t *buffer, uint32_t *codepoints, uint32_t *clusters, uint32_t *before, uint32_t *after, unsigned *length, unsigned *beforeLength, unsigned *afterLength, unsigned *content)
{
  *length = buffer->len;
  for (unsigned i = 0; i < buffer->len; i++) { codepoints[i] = buffer->info[i].codepoint; clusters[i] = buffer->info[i].cluster; }
  *beforeLength = buffer->context_len[0];
  *afterLength = buffer->context_len[1];
  for (unsigned i = 0; i < buffer->context_len[0]; i++) before[i] = buffer->context[0][i];
  for (unsigned i = 0; i < buffer->context_len[1]; i++) after[i] = buffer->context[1][i];
  *content = buffer->content_type;
}

FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_buffer_seed (hb_buffer_t *buffer, const uint32_t *codepoints, const uint32_t *clusters, unsigned length, const int32_t *positions, int havePositions, const uint32_t *before, unsigned beforeLength, const uint32_t *after, unsigned afterLength, unsigned content);
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_buffer_have_positions (hb_buffer_t *buffer);
FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_buffer_positions (hb_buffer_t *buffer, int32_t *positions);

FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_buffer_seed (hb_buffer_t *buffer, const uint32_t *codepoints, const uint32_t *clusters, unsigned length, const int32_t *positions, int havePositions, const uint32_t *before, unsigned beforeLength, const uint32_t *after, unsigned afterLength, unsigned content)
{
  buffer->clear ();
  if (!buffer->ensure (length)) return;
  buffer->len = length;
  for (unsigned i = 0; i < length; i++)
  {
    buffer->info[i] = hb_glyph_info_t ();
    buffer->info[i].codepoint = codepoints[i];
    buffer->info[i].cluster = clusters[i];
  }
  if (havePositions)
  {
    buffer->clear_positions ();
    for (unsigned i = 0; i < length; i++)
    {
      buffer->pos[i].x_advance = positions[i * 5];
      buffer->pos[i].y_advance = positions[i * 5 + 1];
      buffer->pos[i].x_offset = positions[i * 5 + 2];
      buffer->pos[i].y_offset = positions[i * 5 + 3];
    }
  }
  buffer->clear_context (0);
  buffer->clear_context (1);
  for (unsigned i = 0; i < beforeLength && i < buffer->CONTEXT_LENGTH; i++) buffer->context[0][i] = before[i];
  for (unsigned i = 0; i < afterLength && i < buffer->CONTEXT_LENGTH; i++) buffer->context[1][i] = after[i];
  buffer->context_len[0] = beforeLength < buffer->CONTEXT_LENGTH ? beforeLength : buffer->CONTEXT_LENGTH;
  buffer->context_len[1] = afterLength < buffer->CONTEXT_LENGTH ? afterLength : buffer->CONTEXT_LENGTH;
  buffer->content_type = (hb_buffer_content_type_t) content;
}

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_buffer_have_positions (hb_buffer_t *buffer)
{ return buffer->have_positions ? 1 : 0; }

FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_buffer_positions (hb_buffer_t *buffer, int32_t *positions)
{
  for (unsigned i = 0; i < buffer->len; i++)
  {
    positions[i * 5] = buffer->pos[i].x_advance;
    positions[i * 5 + 1] = buffer->pos[i].y_advance;
    positions[i * 5 + 2] = buffer->pos[i].x_offset;
    positions[i * 5 + 3] = buffer->pos[i].y_offset;
    positions[i * 5 + 4] = (int32_t) buffer->pos[i].var.i32;
  }
}

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_buffer_run_script (hb_buffer_t *b, int initial_max_ops, const uint32_t *ops, unsigned n);
FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_buffer_masks (hb_buffer_t *b, uint32_t *masks);

/* Executes the P3.S09 op script against a seeded buffer. Opcodes are the uint32 stream
 * "opcode, args…" documented in phase-3-buffer-unicode.md: 1 ClearOutput, 2 NextGlyph,
 * 3 ReplaceGlyph g, 4 OutputGlyph g, 5 SkipGlyph, 6 DeleteGlyph, 7 CopyGlyph, 8 MoveTo i,
 * 9 MergeClusters s e, 10 MergeOutClusters s e, 11 UnsafeToBreak s e, 12 UnsafeToConcat s e,
 * 13 UnsafeToBreakFromOutbuffer s e, 14 UnsafeToConcatFromOutbuffer s e, 15 Sync,
 * 16 ReplaceGlyphs nIn nOut g…, 17 SetMasks v m cs ce, 18 NextGlyphs n, 19 SafeToInsertTatweel s e.
 *
 * Every precondition the upstream functions would assert, and every array index they would
 * read, is checked here against the buffer's real idx, len, out_len and have_output before the
 * call. When op k+1 (1-based) cannot run, this stops and returns -(k+1); it returns 0 or 1
 * (b->successful) only when the whole script ran. */
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_buffer_run_script (hb_buffer_t *b, int initial_max_ops, const uint32_t *ops, unsigned n)
{
  if (initial_max_ops)
    b->max_ops = initial_max_ops;
  b->enter ();

  unsigned i = 0;   /* next stream element */
  unsigned k = 0;   /* ops executed in full */
  while (i < n)
  {
    unsigned opcode = ops[i];
    switch (opcode)
    {
    case 1: /* ClearOutput */
      i += 1;
      b->clear_output ();
      break;
    case 2: /* NextGlyph */
      i += 1;
      if (b->idx >= b->len) return -(int)(k + 1);
      (void) b->next_glyph ();
      break;
    case 3: /* ReplaceGlyph g */
      if (i + 2 > n) return -(int)(k + 1);
      if (!b->have_output || b->idx + 1 > b->len) return -(int)(k + 1);
      (void) b->replace_glyph (ops[i + 1]);
      i += 2;
      break;
    case 4: /* OutputGlyph g */
      if (i + 2 > n) return -(int)(k + 1);
      if (!b->have_output || b->idx > b->len || (b->idx == b->len && b->out_len == 0)) return -(int)(k + 1);
      (void) b->output_glyph (ops[i + 1]);
      i += 2;
      break;
    case 5: /* SkipGlyph */
      i += 1;
      if (b->idx >= b->len) return -(int)(k + 1);
      b->skip_glyph ();
      break;
    case 6: /* DeleteGlyph */
      i += 1;
      if (b->idx >= b->len) return -(int)(k + 1);
      b->delete_glyph ();
      break;
    case 7: /* CopyGlyph */
      i += 1;
      if (!b->have_output || b->idx >= b->len) return -(int)(k + 1);
      (void) b->copy_glyph ();
      break;
    case 8: /* MoveTo i */
      {
        unsigned target;
        if (i + 2 > n) return -(int)(k + 1);
        target = ops[i + 1];
        if (!b->have_output)
        {
          if (target > b->len) return -(int)(k + 1);
        }
        else if (target > b->out_len + (b->len - b->idx)) return -(int)(k + 1);
        (void) b->move_to (target);
        i += 2;
      }
      break;
    case 9: /* MergeClusters s e */
      {
        unsigned s, e;
        if (i + 3 > n) return -(int)(k + 1);
        s = ops[i + 1];
        e = ops[i + 2];
        if (s >= e || e > b->len) return -(int)(k + 1);
        b->merge_clusters (s, e);
        i += 3;
      }
      break;
    case 10: /* MergeOutClusters s e */
      {
        unsigned s, e;
        if (i + 3 > n) return -(int)(k + 1);
        s = ops[i + 1];
        e = ops[i + 2];
        if (!b->have_output || s >= e || e > b->out_len) return -(int)(k + 1);
        b->merge_out_clusters (s, e);
        i += 3;
      }
      break;
    case 11: /* UnsafeToBreak s e */
    case 12: /* UnsafeToConcat s e */
    case 19: /* SafeToInsertTatweel s e */
      {
        unsigned s, e, clamped;
        if (i + 3 > n) return -(int)(k + 1);
        s = ops[i + 1];
        e = ops[i + 2];
        clamped = e > b->len ? b->len : e;
        if (s > clamped) return -(int)(k + 1);
        if (opcode == 11) b->unsafe_to_break (s, e);
        else if (opcode == 12) b->unsafe_to_concat (s, e);
        else b->safe_to_insert_tatweel (s, e);
        i += 3;
      }
      break;
    case 13: /* UnsafeToBreakFromOutbuffer s e */
    case 14: /* UnsafeToConcatFromOutbuffer s e */
      {
        unsigned s, e, clamped;
        if (i + 3 > n) return -(int)(k + 1);
        s = ops[i + 1];
        e = ops[i + 2];
        clamped = e > b->len ? b->len : e;
        if (!b->have_output || s > b->out_len || b->idx > clamped) return -(int)(k + 1);
        if (opcode == 13) b->unsafe_to_break_from_outbuffer (s, e);
        else b->unsafe_to_concat_from_outbuffer (s, e);
        i += 3;
      }
      break;
    case 15: /* Sync */
      i += 1;
      if (!b->have_output || b->idx > b->len) return -(int)(k + 1);
      b->sync ();
      break;
    case 16: /* ReplaceGlyphs nIn nOut g… */
      {
        unsigned n_in, n_out;
        if (i + 3 > n) return -(int)(k + 1);
        n_in = ops[i + 1];
        n_out = ops[i + 2];
        if (i + 3 + n_out > n) return -(int)(k + 1);
        if (!b->have_output || b->idx + n_in > b->len) return -(int)(k + 1);
        if (b->idx == b->len && b->out_len == 0 && n_out > 0) return -(int)(k + 1);
        (void) b->replace_glyphs (n_in, n_out, ops + i + 3);
        i += 3 + n_out;
      }
      break;
    case 17: /* SetMasks v m cs ce */
      if (i + 5 > n) return -(int)(k + 1);
      b->set_masks (ops[i + 1], ops[i + 2], ops[i + 3], ops[i + 4]);
      i += 5;
      break;
    case 18: /* NextGlyphs n */
      {
        unsigned count;
        if (i + 2 > n) return -(int)(k + 1);
        count = ops[i + 1];
        if (b->idx + count > b->len) return -(int)(k + 1);
        (void) b->next_glyphs (count);
        i += 2;
      }
      break;
    default:
      return -(int)(k + 1);
    }
    k++;
  }

  /* Finish: sync the out-buffer if one is open. */
  if (b->have_output)
  {
    if (b->idx > b->len) return -(int)(k + 1);
    b->sync ();
  }
  return b->successful ? 1 : 0;
}

/* Reads the glyph masks of a buffer, for the P3.S09 differential. */
FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_buffer_masks (hb_buffer_t *buffer, uint32_t *masks)
{
  for (unsigned i = 0; i < buffer->len; i++)
    masks[i] = buffer->info[i].mask;
}

FARFBUZZ_ORACLE_EXPORT float farfbuzz_oracle_hvar_delta (hb_face_t *face, const int *coords, unsigned num_coords, hb_codepoint_t glyph);
FARFBUZZ_ORACLE_EXPORT float farfbuzz_oracle_vvar_delta (hb_face_t *face, const int *coords, unsigned num_coords, hb_codepoint_t glyph);
FARFBUZZ_ORACLE_EXPORT float farfbuzz_oracle_vorg_delta (hb_face_t *face, const int *coords, unsigned num_coords, hb_codepoint_t glyph);
FARFBUZZ_ORACLE_EXPORT float farfbuzz_oracle_mvar_delta (hb_face_t *face, const int *coords, unsigned num_coords, hb_tag_t tag);
FARFBUZZ_ORACLE_EXPORT float farfbuzz_oracle_gdef_delta (hb_face_t *face, const int *coords, unsigned num_coords, unsigned var_idx);
FARFBUZZ_ORACLE_EXPORT double farfbuzz_oracle_gdef_delta_double (hb_face_t *face, const int *coords, unsigned num_coords, unsigned var_idx);

/* Raw float deltas, compared bit for bit (P7.S03). */
FARFBUZZ_ORACLE_EXPORT float
farfbuzz_oracle_hvar_delta (hb_face_t *face, const int *coords, unsigned num_coords, hb_codepoint_t glyph)
{ return face->table.hmtx->var_table->get_advance_delta_unscaled (glyph, coords, num_coords); }
FARFBUZZ_ORACLE_EXPORT float
farfbuzz_oracle_vvar_delta (hb_face_t *face, const int *coords, unsigned num_coords, hb_codepoint_t glyph)
{ return face->table.vmtx->var_table->get_advance_delta_unscaled (glyph, coords, num_coords); }
FARFBUZZ_ORACLE_EXPORT float
farfbuzz_oracle_vorg_delta (hb_face_t *face, const int *coords, unsigned num_coords, hb_codepoint_t glyph)
{ return face->table.vmtx->var_table->get_vorg_delta_unscaled (glyph, coords, num_coords); }
FARFBUZZ_ORACLE_EXPORT float
farfbuzz_oracle_mvar_delta (hb_face_t *face, const int *coords, unsigned num_coords, hb_tag_t tag)
{ return face->table.MVAR->get_var (tag, coords, num_coords); }
FARFBUZZ_ORACLE_EXPORT float
farfbuzz_oracle_gdef_delta (hb_face_t *face, const int *coords, unsigned num_coords, unsigned var_idx)
{ return face->table.GDEF->table->get_var_store ().get_delta (var_idx, coords, num_coords); }
FARFBUZZ_ORACLE_EXPORT double
farfbuzz_oracle_gdef_delta_double (hb_face_t *face, const int *coords, unsigned num_coords, unsigned var_idx)
{ return face->table.GDEF->table->get_var_store ().get_delta<double> (var_idx, coords, num_coords); }

/* AAT lookup and class-table shims (P8.S01). Each runs upstream's own sanitize on the
 * given bytes first and never reads past the length it was given. */

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_aat_lookup_sanitize (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_aat_lookup_get_value (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs, unsigned glyph, unsigned *found);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_aat_lookup_get_value_or_null (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs, unsigned glyph);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_aat_lookup_get_class (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs, unsigned glyph, unsigned out_of_range);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_aat_lookup_collect_glyphs (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs, uint32_t *out, unsigned capacity);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_aat_class_table_get_class (const unsigned char *data, unsigned length, unsigned wide, unsigned glyph, unsigned out_of_range);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_aat_class_table_collect_glyphs (const unsigned char *data, unsigned length, unsigned wide, uint32_t *out, unsigned capacity);
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_aat_class_table_sanitize (const unsigned char *data, unsigned length, unsigned wide);

template <typename T>
static unsigned
aat_lookup_get_value_impl (const unsigned char *data, unsigned length, unsigned num_glyphs, unsigned glyph, unsigned *found)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *l = (const AAT::Lookup<T> *) data;
  if (!l->sanitize (&c)) { *found = 0; return 0; }
  const T *v = l->get_value (glyph, num_glyphs);
  if (!v) { *found = 0; return 0; }
  *found = 1;
  return (unsigned) *v;
}

template <typename T>
static unsigned
aat_lookup_get_value_or_null_impl (const unsigned char *data, unsigned length, unsigned num_glyphs, unsigned glyph)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *l = (const AAT::Lookup<T> *) data;
  if (!l->sanitize (&c)) return 0;
  return (unsigned) l->get_value_or_null (glyph, num_glyphs);
}

template <typename T>
static unsigned
aat_lookup_get_class_impl (const unsigned char *data, unsigned length, unsigned num_glyphs, unsigned glyph, unsigned out_of_range)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *l = (const AAT::Lookup<T> *) data;
  if (!l->sanitize (&c)) return out_of_range;
  return (unsigned) l->get_class (glyph, num_glyphs, out_of_range);
}

template <typename T>
static unsigned
aat_lookup_collect_impl (const unsigned char *data, unsigned length, unsigned num_glyphs, uint32_t *out, unsigned capacity)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *l = (const AAT::Lookup<T> *) data;
  if (!l->sanitize (&c)) return 0;
  hb_set_t *set = hb_set_create ();
  l->collect_glyphs (*set, num_glyphs);
  hb_codepoint_t first = HB_SET_VALUE_INVALID, last = HB_SET_VALUE_INVALID;
  unsigned count = 0;
  while (count < capacity && hb_set_next_range (set, &first, &last))
  {
    out[count * 2] = first;
    out[count * 2 + 1] = last;
    count++;
  }
  hb_set_destroy (set);
  return count;
}

template <typename HBUCHAR>
static unsigned
aat_class_table_get_class_impl (const unsigned char *data, unsigned length, unsigned glyph, unsigned out_of_range)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  auto *t = (const AAT::ClassTable<HBUCHAR> *) data;
  if (!t->sanitize (&c)) return out_of_range;
  return (unsigned) t->get_class (glyph, out_of_range);
}

template <typename HBUCHAR>
static unsigned
aat_class_table_collect_impl (const unsigned char *data, unsigned length, uint32_t *out, unsigned capacity)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  auto *t = (const AAT::ClassTable<HBUCHAR> *) data;
  if (!t->sanitize (&c)) return 0;
  hb_set_t *set = hb_set_create ();
  t->collect_glyphs (*set, 0);
  hb_codepoint_t first = HB_SET_VALUE_INVALID, last = HB_SET_VALUE_INVALID;
  unsigned count = 0;
  while (count < capacity && hb_set_next_range (set, &first, &last))
  {
    out[count * 2] = first;
    out[count * 2 + 1] = last;
    count++;
  }
  hb_set_destroy (set);
  return count;
}

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_aat_lookup_sanitize (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  switch (value_size)
  {
    case 1: { auto *l = (const AAT::Lookup<OT::HBUINT8> *) data; return l->sanitize (&c); }
    case 2: { auto *l = (const AAT::Lookup<OT::HBUINT16> *) data; return l->sanitize (&c); }
    case 4: { auto *l = (const AAT::Lookup<OT::HBUINT32> *) data; return l->sanitize (&c); }
    default: return 0;
  }
}

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_aat_lookup_get_value (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs, unsigned glyph, unsigned *found)
{
  switch (value_size)
  {
    case 1: return aat_lookup_get_value_impl<OT::HBUINT8> (data, length, num_glyphs, glyph, found);
    case 2: return aat_lookup_get_value_impl<OT::HBUINT16> (data, length, num_glyphs, glyph, found);
    case 4: return aat_lookup_get_value_impl<OT::HBUINT32> (data, length, num_glyphs, glyph, found);
    default: *found = 0; return 0;
  }
}

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_aat_lookup_get_value_or_null (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs, unsigned glyph)
{
  switch (value_size)
  {
    case 1: return aat_lookup_get_value_or_null_impl<OT::HBUINT8> (data, length, num_glyphs, glyph);
    case 2: return aat_lookup_get_value_or_null_impl<OT::HBUINT16> (data, length, num_glyphs, glyph);
    case 4: return aat_lookup_get_value_or_null_impl<OT::HBUINT32> (data, length, num_glyphs, glyph);
    default: return 0;
  }
}

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_aat_lookup_get_class (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs, unsigned glyph, unsigned out_of_range)
{
  switch (value_size)
  {
    case 1: return aat_lookup_get_class_impl<OT::HBUINT8> (data, length, num_glyphs, glyph, out_of_range);
    case 2: return aat_lookup_get_class_impl<OT::HBUINT16> (data, length, num_glyphs, glyph, out_of_range);
    case 4: return aat_lookup_get_class_impl<OT::HBUINT32> (data, length, num_glyphs, glyph, out_of_range);
    default: return out_of_range;
  }
}

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_aat_lookup_collect_glyphs (const unsigned char *data, unsigned length, unsigned value_size, unsigned num_glyphs, uint32_t *out, unsigned capacity)
{
  switch (value_size)
  {
    case 1: return aat_lookup_collect_impl<OT::HBUINT8> (data, length, num_glyphs, out, capacity);
    case 2: return aat_lookup_collect_impl<OT::HBUINT16> (data, length, num_glyphs, out, capacity);
    case 4: return aat_lookup_collect_impl<OT::HBUINT32> (data, length, num_glyphs, out, capacity);
    default: return 0;
  }
}

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_aat_class_table_get_class (const unsigned char *data, unsigned length, unsigned wide, unsigned glyph, unsigned out_of_range)
{
  return wide ? aat_class_table_get_class_impl<OT::HBUINT16> (data, length, glyph, out_of_range)
              : aat_class_table_get_class_impl<OT::HBUINT8> (data, length, glyph, out_of_range);
}

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_aat_class_table_collect_glyphs (const unsigned char *data, unsigned length, unsigned wide, uint32_t *out, unsigned capacity)
{
  return wide ? aat_class_table_collect_impl<OT::HBUINT16> (data, length, out, capacity)
              : aat_class_table_collect_impl<OT::HBUINT8> (data, length, out, capacity);
}

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_aat_class_table_sanitize (const unsigned char *data, unsigned length, unsigned wide)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  if (wide)
  {
    auto *t = (const AAT::ClassTable<OT::HBUINT16> *) data;
    return t->sanitize (&c);
  }
  auto *t = (const AAT::ClassTable<OT::HBUINT8> *) data;
  return t->sanitize (&c);
}

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_set_digest_may_have (const uint32_t *ops, unsigned opCount, uint32_t query);

/* Builds an hb_set_digest_t from an add/add_range op stream, then answers may_have.
 * Each op is three uint32_t: kind (0 = add a, 1 = add_range a b), a, b. */
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_set_digest_may_have (const uint32_t *ops, unsigned opCount, uint32_t query)
{
  hb_set_digest_t digest;
  digest.init ();
  for (unsigned i = 0; i < opCount; i++)
  {
    unsigned kind = ops[i * 3];
    uint32_t a = ops[i * 3 + 1];
    uint32_t b = ops[i * 3 + 2];
    if (kind == 0)
      digest.add (a);
    else
      digest.add_range (a, b);
  }
  return digest.may_have (query) ? 1 : 0;
}

FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_gdef_glyph_props (hb_face_t *face, hb_codepoint_t g);
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_gdef_mark_set_covers (hb_face_t *face, unsigned set, hb_codepoint_t g);

/* GDEF glyph props of a glyph, through the GDEF accelerator (hb-ot-layout.cc). */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_gdef_glyph_props (hb_face_t *face, hb_codepoint_t g)
{
  return face->table.GDEF->get_glyph_props (g);
}

/* Whether a mark set covers a glyph; an out-of-range set index is not covered,
 * mirroring sanitize_lookup_props before the accelerator's mark_set_covers. */
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_gdef_mark_set_covers (hb_face_t *face, unsigned set, hb_codepoint_t g)
{
  unsigned props = OT::LookupFlag::UseMarkFilteringSet | (set << 16);
  if (!(face->table.GDEF->sanitize_lookup_props (props) & OT::LookupFlag::UseMarkFilteringSet))
    return 0;
  return face->table.GDEF->mark_set_covers (set, g) ? 1 : 0;
}

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_table_find_feature (hb_face_t *face, hb_tag_t table_tag, hb_tag_t feature_tag, unsigned *feature_index);

/* Finds a feature's index at table level (hb_ot_layout_table_find_feature, HB_INTERNAL). */
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_table_find_feature (hb_face_t *face, hb_tag_t table_tag, hb_tag_t feature_tag, unsigned *feature_index)
{
  return hb_ot_layout_table_find_feature (face, table_tag, feature_tag, feature_index) ? 1 : 0;
}

FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_coverage (const uint8_t *bytes, unsigned length, const uint32_t *glyphs, unsigned glyphCount, unsigned *sanitized, int32_t *coverages, uint32_t *iterGlyphs, unsigned iterCapacity, unsigned *iterCount, uint8_t *mayHave);
FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_classdef (const uint8_t *bytes, unsigned length, const uint32_t *glyphs, unsigned glyphCount, const uint32_t *collectClasses, unsigned collectCount, unsigned *sanitized, uint32_t *classes, uint32_t *rangeFirsts, uint32_t *rangeLasts, unsigned rangeCapacity, unsigned *rangeCounts);

/* Runs upstream's sanitize on a Coverage's bytes, then reports the verdict, get_coverage for the
 * sample glyphs, the whole iter_t iteration and the digest's may_have. Never reads past the length. */
FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_coverage (const uint8_t *bytes, unsigned length, const uint32_t *glyphs, unsigned glyphCount,
                          unsigned *sanitized, int32_t *coverages, uint32_t *iterGlyphs, unsigned iterCapacity,
                          unsigned *iterCount, uint8_t *mayHave)
{
  *sanitized = 0;
  *iterCount = 0;
  for (unsigned i = 0; i < glyphCount; i++) { coverages[i] = -1; mayHave[i] = 0; }
  if (!length) return;
  hb_blob_t *blob = hb_blob_create ((const char *) bytes, length, HB_MEMORY_MODE_READONLY, nullptr, nullptr);
  hb_sanitize_context_t c (blob);
  const OT::Layout::Common::Coverage *coverage = reinterpret_cast<const OT::Layout::Common::Coverage *> (c.start);
  bool sane = coverage->sanitize (&c);
  *sanitized = sane ? 1 : 0;
  if (sane)
  {
    for (unsigned i = 0; i < glyphCount; i++)
      coverages[i] = (int32_t) coverage->get_coverage (glyphs[i]);
    auto it = coverage->iter ();
    unsigned count = 0;
    while (it.__more__ () && count < iterCapacity)
    {
      iterGlyphs[count++] = it.get_glyph ();
      it.__next__ ();
    }
    *iterCount = count;
    hb_set_digest_t digest;
    digest.init ();
    coverage->collect_coverage (&digest);
    for (unsigned i = 0; i < glyphCount; i++)
      mayHave[i] = digest.may_have (glyphs[i]) ? 1 : 0;
  }
  hb_blob_destroy (blob);
}

/* Runs upstream's sanitize on a ClassDef's bytes, then reports the verdict, get_class for the sample
 * glyphs and collect_class for each requested class as set ranges. Never reads past the length. */
FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_classdef (const uint8_t *bytes, unsigned length, const uint32_t *glyphs, unsigned glyphCount,
                          const uint32_t *collectClasses, unsigned collectCount, unsigned *sanitized,
                          uint32_t *classes, uint32_t *rangeFirsts, uint32_t *rangeLasts, unsigned rangeCapacity,
                          unsigned *rangeCounts)
{
  *sanitized = 0;
  for (unsigned i = 0; i < glyphCount; i++) classes[i] = 0;
  for (unsigned k = 0; k < collectCount; k++) rangeCounts[k] = 0;
  if (!length) return;
  hb_blob_t *blob = hb_blob_create ((const char *) bytes, length, HB_MEMORY_MODE_READONLY, nullptr, nullptr);
  hb_sanitize_context_t c (blob);
  const OT::ClassDef *classdef = reinterpret_cast<const OT::ClassDef *> (c.start);
  bool sane = classdef->sanitize (&c);
  *sanitized = sane ? 1 : 0;
  if (sane)
  {
    for (unsigned i = 0; i < glyphCount; i++)
      classes[i] = classdef->get_class (glyphs[i]);
    unsigned written = 0;
    for (unsigned k = 0; k < collectCount; k++)
    {
      rangeCounts[k] = 0;
      hb_set_t *set = hb_set_create ();
      classdef->collect_class (set, collectClasses[k]);
      hb_codepoint_t first = 0;
      hb_codepoint_t last = HB_SET_VALUE_INVALID;
      while (written < rangeCapacity && hb_set_next_range (set, &first, &last))
      {
        rangeFirsts[written] = first;
        rangeLasts[written] = last;
        written++;
        rangeCounts[k]++;
      }
      hb_set_destroy (set);
    }
  }
  hb_blob_destroy (blob);
}

/* AAT state-table shims (P8.S02). Each runs upstream's own sanitize on the
 * given bytes first and never reads past the length it was given. */

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_aat_state_table_sanitize (const unsigned char *data, unsigned length, unsigned extended, unsigned entry_data_words, unsigned num_glyphs, unsigned *num_entries, int *min_state, int *max_state);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_aat_state_table_get_class (const unsigned char *data, unsigned length, unsigned extended, unsigned entry_data_words, unsigned num_glyphs, unsigned glyph);
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_aat_state_table_get_entry (const unsigned char *data, unsigned length, unsigned extended, unsigned entry_data_words, unsigned num_glyphs, int state, unsigned klass, int *new_state, unsigned *flags, unsigned *data0, unsigned *data1);

/* One 16-bit extra data word per entry, the morx-ligature entry shape. */
struct AatStateEntryData1
{
  HBUINT16 word;
  public:
  DEFINE_SIZE_STATIC (2);
};

template <typename Extra>
struct AatStateEntryDataReader
{
  static unsigned data0 (const AAT::Entry<Extra> &) { return 0; }
  static unsigned data1 (const AAT::Entry<Extra> &) { return 0; }
};
template <>
struct AatStateEntryDataReader<AatStateEntryData1>
{
  static unsigned data0 (const AAT::Entry<AatStateEntryData1> &entry) { return (unsigned) entry.data.word; }
  static unsigned data1 (const AAT::Entry<AatStateEntryData1> &) { return 0; }
};

template <typename Types, typename Extra>
static int
aat_state_table_sanitize_impl (const unsigned char *data, unsigned length, unsigned num_glyphs,
                               unsigned *num_entries_out, int *min_state_out, int *max_state_out)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *t = (const AAT::StateTable<Types, Extra> *) data;
  unsigned num_entries = 0;
  if (!t->sanitize (&c, &num_entries))
  {
    *num_entries_out = 0;
    *min_state_out = 0;
    *max_state_out = 0;
    return 0;
  }
  *num_entries_out = num_entries;
  const auto *entries = t->get_entries ();
  int min_state = 0, max_state = 0;
  for (unsigned i = 0; i < num_entries; i++)
  {
    int new_state = t->new_state (entries[i].newState);
    min_state = hb_min (min_state, new_state);
    max_state = hb_max (max_state, new_state);
  }
  *min_state_out = min_state;
  *max_state_out = max_state;
  return 1;
}

template <typename Types, typename Extra>
static unsigned
aat_state_table_get_class_impl (const unsigned char *data, unsigned length, unsigned num_glyphs, unsigned glyph)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *t = (const AAT::StateTable<Types, Extra> *) data;
  unsigned num_entries = 0;
  if (!t->sanitize (&c, &num_entries)) return AAT::CLASS_OUT_OF_BOUNDS;
  return t->get_class (glyph, num_glyphs);
}

template <typename Types, typename Extra>
static int
aat_state_table_get_entry_impl (const unsigned char *data, unsigned length, unsigned num_glyphs,
                                int state, unsigned klass,
                                int *new_state, unsigned *flags, unsigned *data0, unsigned *data1)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *t = (const AAT::StateTable<Types, Extra> *) data;
  unsigned num_entries = 0;
  if (!t->sanitize (&c, &num_entries)) return 0;
  const auto &entry = t->get_entry (state, klass);
  *new_state = t->new_state (entry.newState);
  *flags = (unsigned) entry.flags;
  *data0 = AatStateEntryDataReader<Extra>::data0 (entry);
  *data1 = AatStateEntryDataReader<Extra>::data1 (entry);
  return 1;
}

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_aat_state_table_sanitize (const unsigned char *data, unsigned length, unsigned extended, unsigned entry_data_words, unsigned num_glyphs, unsigned *num_entries, int *min_state, int *max_state)
{
  if (extended)
  {
    if (entry_data_words == 0)
      return aat_state_table_sanitize_impl<AAT::ExtendedTypes, void> (data, length, num_glyphs, num_entries, min_state, max_state);
    return aat_state_table_sanitize_impl<AAT::ExtendedTypes, AatStateEntryData1> (data, length, num_glyphs, num_entries, min_state, max_state);
  }
  if (entry_data_words == 0)
    return aat_state_table_sanitize_impl<AAT::ObsoleteTypes, void> (data, length, num_glyphs, num_entries, min_state, max_state);
  return aat_state_table_sanitize_impl<AAT::ObsoleteTypes, AatStateEntryData1> (data, length, num_glyphs, num_entries, min_state, max_state);
}

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_aat_state_table_get_class (const unsigned char *data, unsigned length, unsigned extended, unsigned entry_data_words, unsigned num_glyphs, unsigned glyph)
{
  if (extended)
  {
    if (entry_data_words == 0)
      return aat_state_table_get_class_impl<AAT::ExtendedTypes, void> (data, length, num_glyphs, glyph);
    return aat_state_table_get_class_impl<AAT::ExtendedTypes, AatStateEntryData1> (data, length, num_glyphs, glyph);
  }
  if (entry_data_words == 0)
    return aat_state_table_get_class_impl<AAT::ObsoleteTypes, void> (data, length, num_glyphs, glyph);
  return aat_state_table_get_class_impl<AAT::ObsoleteTypes, AatStateEntryData1> (data, length, num_glyphs, glyph);
}

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_aat_state_table_get_entry (const unsigned char *data, unsigned length, unsigned extended, unsigned entry_data_words, unsigned num_glyphs, int state, unsigned klass, int *new_state, unsigned *flags, unsigned *data0, unsigned *data1)
{
  if (extended)
  {
    if (entry_data_words == 0)
      return aat_state_table_get_entry_impl<AAT::ExtendedTypes, void> (data, length, num_glyphs, state, klass, new_state, flags, data0, data1);
    return aat_state_table_get_entry_impl<AAT::ExtendedTypes, AatStateEntryData1> (data, length, num_glyphs, state, klass, new_state, flags, data0, data1);
  }
  if (entry_data_words == 0)
    return aat_state_table_get_entry_impl<AAT::ObsoleteTypes, void> (data, length, num_glyphs, state, klass, new_state, flags, data0, data1);
  return aat_state_table_get_entry_impl<AAT::ObsoleteTypes, AatStateEntryData1> (data, length, num_glyphs, state, klass, new_state, flags, data0, data1);
}

FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_arabic_joining_type_raw (hb_codepoint_t u);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_arabic_shaping (hb_codepoint_t u, unsigned form);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_arabic_ligatures (unsigned which, uint16_t *out, unsigned capacity);

/* Raw Arabic joining type from hb-ot-shaper-arabic-table.hh, before the general-category
 * resolution of get_joining_type (which farfbuzz_oracle_arabic_joining_type exposes). */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_arabic_joining_type_raw (hb_codepoint_t u)
{
  return joining_type (u);
}

/* Shaping-table lookup: the presentation form for a code point and form (0 initial, 1 medial,
 * 2 final, 3 isolated), or 0 outside [SHAPING_TABLE_FIRST, SHAPING_TABLE_LAST]. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_arabic_shaping (hb_codepoint_t u, unsigned form)
{
  if (u < SHAPING_TABLE_FIRST || u > SHAPING_TABLE_LAST)
    return 0;
  return shaping_table[u - SHAPING_TABLE_FIRST][form];
}

/* Serialises one of the three Arabic ligature tables: which 0 = ligature_3_table, 1 = ligature_table,
 * 2 = ligature_mark_table. Per set: first, then per non-padding entry the components then the ligature.
 * Returns the number of uint16 written, stopping early when capacity runs out. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_arabic_ligatures (unsigned which, uint16_t *out, unsigned capacity)
{
  unsigned written = 0;
#define FARFBUZZ_WRITE(value) do { if (written >= capacity) return written; out[written++] = (value); } while (0)
  if (which == 0)
  {
    for (unsigned s = 0; s < ARRAY_LENGTH (ligature_3_table); s++)
    {
      FARFBUZZ_WRITE (ligature_3_table[s].first);
      for (unsigned i = 0; i < ARRAY_LENGTH (ligature_3_table[s].ligatures); i++)
      {
        if (!ligature_3_table[s].ligatures[i].ligature)
          continue;
        FARFBUZZ_WRITE (ligature_3_table[s].ligatures[i].components[0]);
        FARFBUZZ_WRITE (ligature_3_table[s].ligatures[i].components[1]);
        FARFBUZZ_WRITE (ligature_3_table[s].ligatures[i].ligature);
      }
    }
  }
  else if (which == 1)
  {
    for (unsigned s = 0; s < ARRAY_LENGTH (ligature_table); s++)
    {
      FARFBUZZ_WRITE (ligature_table[s].first);
      for (unsigned i = 0; i < ARRAY_LENGTH (ligature_table[s].ligatures); i++)
      {
        if (!ligature_table[s].ligatures[i].ligature)
          continue;
        FARFBUZZ_WRITE (ligature_table[s].ligatures[i].components[0]);
        FARFBUZZ_WRITE (ligature_table[s].ligatures[i].ligature);
      }
    }
  }
  else if (which == 2)
  {
    for (unsigned s = 0; s < ARRAY_LENGTH (ligature_mark_table); s++)
    {
      FARFBUZZ_WRITE (ligature_mark_table[s].first);
      for (unsigned i = 0; i < ARRAY_LENGTH (ligature_mark_table[s].ligatures); i++)
      {
        if (!ligature_mark_table[s].ligatures[i].ligature)
          continue;
        FARFBUZZ_WRITE (ligature_mark_table[s].ligatures[i].components[0]);
        FARFBUZZ_WRITE (ligature_mark_table[s].ligatures[i].ligature);
      }
    }
  }
#undef FARFBUZZ_WRITE
  return written;
}

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_has_arabic_joining (uint32_t script);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_arabic_win1256 (uint8_t *out, unsigned capacity);

/* Whether a script has data in the Arabic joining table (hb-ot-shaper-arabic-joining-list.hh). */
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_has_arabic_joining (uint32_t script)
{
  return has_arabic_joining ((hb_script_t) script) ? 1 : 0;
}

/* Copies the Windows-1256 GSUB lookup table bytes; returns its byte size. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_arabic_win1256 (uint8_t *out, unsigned capacity)
{
  unsigned size = (unsigned) sizeof (arabic_win1256_gsub_lookups);
  if (capacity >= size)
    memcpy (out, &arabic_win1256_gsub_lookups, size);
  return size;
}

FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_glyf_points (hb_font_t *font, hb_codepoint_t gid, unsigned phantom_only,
                                                             float *xy, unsigned char *flags, unsigned capacity);

/* All points of a glyph after Glyph::get_points, phantoms last (P7.S07).  Returns the point count,
 * 0xFFFFFFFF when get_points fails.  Writes at most capacity points. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_glyf_points (hb_font_t *font, hb_codepoint_t gid, unsigned phantom_only,
                             float *xy, unsigned char *flags, unsigned capacity)
{
  const OT::glyf_accelerator_t &glyf = *font->face->table.glyf;
  hb_glyf_scratch_t *scratch = glyf.acquire_scratch ();
  if (!scratch) return 0xFFFFFFFFu;
  contour_point_vector_t &all_points = scratch->all_points;
  all_points.clear ();
  scratch->comp_points.clear ();
  hb_array_t<const int> coords = hb_array (font->coords, font->has_nonzero_coords ? font->num_coords : 0);
  bool ok = glyf.glyph_for_gid (gid).get_points (font, glyf, all_points, *scratch, nullptr, nullptr, nullptr,
                                                 true, true, (bool) phantom_only, coords);
  unsigned n = ok ? all_points.length : 0xFFFFFFFFu;
  for (unsigned i = 0; ok && i < n && i < capacity; i++)
  {
    xy[2 * i] = all_points.arrayZ[i].x; xy[2 * i + 1] = all_points.arrayZ[i].y;
    flags[i] = all_points.arrayZ[i].flag | (all_points.arrayZ[i].is_end_point ? 0x80u : 0u);
  }
  glyf.release_scratch (scratch);
  return n;
}

FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_vowel_constraints (uint32_t script, unsigned buffer_flags, const uint32_t *text, unsigned len, uint32_t *out_codepoints, uint32_t *out_clusters, unsigned capacity);

/* Runs _hb_preprocess_text_vowel_constraints on a fresh LTR buffer seeded from text, then copies
 * the resulting codepoints and clusters. Returns the result length. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_vowel_constraints (uint32_t script, unsigned buffer_flags, const uint32_t *text, unsigned len, uint32_t *out_codepoints, uint32_t *out_clusters, unsigned capacity)
{
  hb_buffer_t *buffer = hb_buffer_create ();
  hb_buffer_add_codepoints (buffer, text, len, 0, len);
  hb_buffer_set_flags (buffer, (hb_buffer_flags_t) buffer_flags);
  hb_buffer_set_script (buffer, (hb_script_t) script);
  hb_buffer_set_direction (buffer, HB_DIRECTION_LTR);
  _hb_preprocess_text_vowel_constraints (nullptr, buffer, nullptr);
  unsigned result = buffer->len;
  for (unsigned i = 0; i < result && i < capacity; i++)
  {
    out_codepoints[i] = buffer->info[i].codepoint;
    out_clusters[i] = buffer->info[i].cluster;
  }
  hb_buffer_destroy (buffer);
  return result;
}

/* Packed point and delta streams (P7.S08). Each shim decompiles a byte stream,
 * reports the verdict, the decoded values and the number of bytes consumed, and
 * never reads past the length it is given. */

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_decompile_points (const unsigned char *data, unsigned length,
                                                             uint32_t *out, unsigned capacity,
                                                             unsigned *out_count, unsigned *consumed);
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_decompile_deltas (const unsigned char *data, unsigned length,
                                                             unsigned count, unsigned start,
                                                             int32_t *out, unsigned capacity, unsigned *consumed);

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_decompile_points (const unsigned char *data, unsigned length,
                                  uint32_t *out, unsigned capacity,
                                  unsigned *out_count, unsigned *consumed)
{
  const OT::HBUINT8 *begin = (const OT::HBUINT8 *) data;
  const OT::HBUINT8 *p = begin;
  const OT::HBUINT8 *end = begin + length;
  hb_vector_t<unsigned> points;
  if (!OT::TupleVariationData<>::decompile_points (p, points, end))
    return 0;
  *out_count = points.length;
  for (unsigned i = 0; i < points.length && i < capacity; i++)
    out[i] = points[i];
  *consumed = (unsigned) (p - begin);
  return 1;
}

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_decompile_deltas (const unsigned char *data, unsigned length,
                                  unsigned count, unsigned start,
                                  int32_t *out, unsigned capacity, unsigned *consumed)
{
  if (count > 0x7FFFFFFFu)
    return 0;
  const OT::HBUINT8 *begin = (const OT::HBUINT8 *) data;
  const OT::HBUINT8 *p = begin;
  const OT::HBUINT8 *end = begin + length;
  hb_vector_t<int> deltas;
  if (!deltas.resize ((int) count))
    return 0;
  if (!OT::TupleValues::decompile<int> (p, deltas, end, false, start))
    return 0;
  *consumed = (unsigned) (p - begin);
  for (unsigned i = 0; i < count && i < capacity; i++)
    out[i] = deltas[i];
  return 1;
}

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_find_syllables (unsigned shaper, const uint8_t *categories,
                                                           const uint8_t *is_unicode_mark, unsigned len,
                                                           uint8_t *out_syllables);

/* Runs one of the four Ragel syllable finders on a buffer whose glyphs carry the given categories.
 * shaper: 0 indic, 1 khmer, 2 myanmar, 3 use. is_unicode_mark is 0/1 per glyph, read by the USE ZWNJ
 * filter only, and may be NULL. Returns 1 if HAS_BROKEN_SYLLABLE was raised, 0 if not, -1 on bad input. */
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_find_syllables (unsigned shaper, const uint8_t *categories,
                                const uint8_t *is_unicode_mark, unsigned len, uint8_t *out_syllables)
{
  if (shaper > 3) return -1;
  hb_buffer_t *buffer = hb_buffer_create ();
  for (unsigned i = 0; i < len; i++)
    hb_buffer_add (buffer, 0x25CCu, i);
  if (!buffer->successful || buffer->len != len) { hb_buffer_destroy (buffer); return -1; }
  hb_glyph_info_t *info = buffer->info;
  for (unsigned i = 0; i < len; i++)
  {
    _hb_glyph_info_set_general_category (&info[i], is_unicode_mark && is_unicode_mark[i]
                                                      ? HB_UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK
                                                      : HB_UNICODE_GENERAL_CATEGORY_OTHER_LETTER);
    info[i].ot_shaper_var_u8_category () = categories[i];
  }
  switch (shaper)
  {
    case 0: find_syllables_indic (buffer); break;
    case 1: find_syllables_khmer (buffer); break;
    case 2: find_syllables_myanmar (buffer); break;
    case 3: find_syllables_use (buffer); break;
  }
  for (unsigned i = 0; i < len; i++)
    out_syllables[i] = info[i].syllable ();
  int broken = !!(buffer->scratch_flags & HB_BUFFER_SCRATCH_FLAG_HAS_BROKEN_SYLLABLE);
  hb_buffer_destroy (buffer);
  return broken;
}

FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_insert_dotted_circles (hb_font_t *font, unsigned buffer_flags, const uint32_t *glyphs, const uint32_t *clusters, const uint8_t *syllables, const uint8_t *categories, unsigned len, unsigned broken_syllable_type, unsigned dottedcircle_category, int repha_category, int dottedcircle_position, uint32_t *out_glyphs, uint32_t *out_clusters, uint8_t *out_syllables, uint8_t *out_categories, uint8_t *out_auxiliary, unsigned capacity);

/* Runs hb_syllabic_insert_dotted_circles on a buffer whose glyphs carry the given clusters,
 * syllables and categories (auxiliary zeroed), raises HAS_BROKEN_SYLLABLE, then copies the
 * resulting glyphs, clusters, syllables, categories and auxiliary bytes. Returns the result
 * length, or 0xFFFFFFFF when the buffer cannot be built. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_insert_dotted_circles (hb_font_t *font, unsigned buffer_flags, const uint32_t *glyphs, const uint32_t *clusters, const uint8_t *syllables, const uint8_t *categories, unsigned len, unsigned broken_syllable_type, unsigned dottedcircle_category, int repha_category, int dottedcircle_position, uint32_t *out_glyphs, uint32_t *out_clusters, uint8_t *out_syllables, uint8_t *out_categories, uint8_t *out_auxiliary, unsigned capacity)
{
  hb_buffer_t *buffer = hb_buffer_create ();
  for (unsigned i = 0; i < len; i++)
    hb_buffer_add (buffer, glyphs[i], clusters[i]);
  if (!buffer->successful || buffer->len != len) { hb_buffer_destroy (buffer); return 0xFFFFFFFFu; }
  hb_buffer_set_flags (buffer, (hb_buffer_flags_t) buffer_flags);
  hb_glyph_info_t *info = buffer->info;
  for (unsigned i = 0; i < len; i++)
  {
    info[i].syllable () = syllables[i];
    info[i].ot_shaper_var_u8_category () = categories[i];
    info[i].ot_shaper_var_u8_auxiliary () = 0x7Fu;
  }
  buffer->scratch_flags |= HB_BUFFER_SCRATCH_FLAG_HAS_BROKEN_SYLLABLE;
  (void) hb_syllabic_insert_dotted_circles (font, buffer, broken_syllable_type, dottedcircle_category, repha_category, dottedcircle_position);
  info = buffer->info;
  unsigned result = buffer->len;
  for (unsigned i = 0; i < result && i < capacity; i++)
  {
    out_glyphs[i] = info[i].codepoint;
    out_clusters[i] = info[i].cluster;
    out_syllables[i] = info[i].syllable ();
    out_categories[i] = info[i].ot_shaper_var_u8_category ();
    out_auxiliary[i] = info[i].ot_shaper_var_u8_auxiliary ();
  }
  hb_buffer_destroy (buffer);
  return result;
}

FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_lookup_digest (hb_face_t *face, hb_tag_t table_tag, unsigned lookup, uint64_t out[6]);

/* The three masks of a lookup accelerator's digest followed by its digest_second
 * (hb_ot_layout_lookup_accelerator_t), for the P5.S06 lookup-digest differential. */
FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_lookup_digest (hb_face_t *face, hb_tag_t table_tag, unsigned lookup, uint64_t out[6])
{
  for (unsigned i = 0; i < 6; i++) out[i] = 0;
  auto *accel = table_tag == HB_OT_TAG_GSUB ? face->table.GSUB->get_accel (lookup) : face->table.GPOS->get_accel (lookup);
  if (!accel) return;
  const uint64_t *digest = reinterpret_cast<const uint64_t *> (&accel->digest);
  const uint64_t *second = reinterpret_cast<const uint64_t *> (&accel->digest_second);
  for (unsigned i = 0; i < 3; i++)
  {
    out[i] = digest[i];
    out[3 + i] = second[i];
  }
}

FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_feature_lookups (hb_face_t *face, unsigned table_index, unsigned feature_index,
                                                                 const int *coords, unsigned num_coords, unsigned *out, unsigned capacity);

/* Lookup indices a feature contributes at coords, as hb_ot_map_builder_t::add_lookups collects them
 * (lookup variations first, then the substituted feature) (P7.S12). Returns the total count; writes at
 * most capacity lookups in ascending order. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_feature_lookups (hb_face_t *face, unsigned table_index, unsigned feature_index,
                                 const int *coords, unsigned num_coords, unsigned *out, unsigned capacity)
{
  hb_ot_shape_plan_key_t key;
  if (!key.init (face, coords, num_coords))
    return 0;
  hb_set_t *s = hb_set_create ();
  if (!key.get_lookup_variations (face, table_index, feature_index, s))
  {
    unsigned start_offset = 0;
    unsigned lookup_indices[128];
    while (true)
    {
      unsigned lookup_count = ARRAY_LENGTH (lookup_indices);
      (void) hb_ot_layout_feature_with_variations_get_lookups (face, table_tags[table_index], feature_index,
                                                               key.variations_index[table_index],
                                                               start_offset, &lookup_count, lookup_indices);
      for (unsigned i = 0; i < lookup_count; i++)
        hb_set_add (s, lookup_indices[i]);
      if (lookup_count == 0)
        break;
      start_offset += lookup_count;
    }
  }
  unsigned count = 0;
  hb_codepoint_t v = HB_SET_VALUE_INVALID;
  while (hb_set_next (s, &v))
  {
    if (count < capacity)
      out[count] = v;
    count++;
  }
  key.fini ();
  hb_set_destroy (s);
  return count;
}

FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_device_delta (hb_face_t *face, const int *coords, unsigned num_coords, unsigned var_idx, int x_scale, int y_scale, int *x_delta, int *y_delta);

/* Device::get_x_delta / get_y_delta for a variation device with the face's GDEF store (P7.S13). */
FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_device_delta (hb_face_t *face, const int *coords, unsigned num_coords, unsigned var_idx, int x_scale, int y_scale, int *x_delta, int *y_delta)
{
  hb_font_t *font = hb_font_create (face);
  hb_ot_font_set_funcs (font);
  hb_font_set_scale (font, x_scale, y_scale);
  hb_font_set_var_coords_normalized (font, coords, num_coords);
  const OT::ItemVariationStore &store = face->table.GDEF->table->get_var_store ();
  alignas(OT::Device) unsigned char device_bytes[sizeof (OT::Device)] = {0};
  device_bytes[0] = (unsigned char) (var_idx >> 24);
  device_bytes[1] = (unsigned char) (var_idx >> 16);
  device_bytes[2] = (unsigned char) (var_idx >> 8);
  device_bytes[3] = (unsigned char) var_idx;
  device_bytes[4] = 0x80; /* 0x8000 big-endian format */
  device_bytes[5] = 0x00;
  const OT::Device *device = reinterpret_cast<const OT::Device *> (device_bytes);
  *x_delta = device->get_x_delta (font, store);
  *y_delta = device->get_y_delta (font, store);
  hb_font_destroy (font);
}

/* AAT feature-map shims (P8.S05). Each runs upstream's own sanitize on the face's tables
 * and never reads past the feature array it is given. */

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_aat_find_feature_mapping (hb_tag_t tag, unsigned *type, unsigned *enable, unsigned *disable);
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_aat_map_compile (hb_face_t *face, unsigned direction, hb_script_t script, hb_language_t language, const hb_feature_t *features, unsigned num_features, uint32_t *out, unsigned capacity);

/* Resolves an OpenType feature tag through hb_aat_layout_find_feature_mapping. Returns 0 when absent. */
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_aat_find_feature_mapping (hb_tag_t tag, unsigned *type, unsigned *enable, unsigned *disable)
{
  const hb_aat_feature_mapping_t *mapping = hb_aat_layout_find_feature_mapping (tag);
  if (!mapping) return 0;
  *type = (unsigned) mapping->aatFeatureType;
  *enable = (unsigned) mapping->selectorToEnable;
  *disable = (unsigned) mapping->selectorToDisable;
  return 1;
}

/* Compiles the AAT map for a face, segment properties and user features, then serializes
 * chain_flags into a flat uint32 array: the chain count, then per chain the range count and
 * each range's flags, cluster_first and cluster_last. Returns the full uint32 size, writing
 * at most capacity entries. */
FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_aat_map_compile (hb_face_t *face, unsigned direction, hb_script_t script, hb_language_t language,
                                 const hb_feature_t *features, unsigned num_features,
                                 uint32_t *out, unsigned capacity)
{
  hb_segment_properties_t props = HB_SEGMENT_PROPERTIES_DEFAULT;
  props.direction = (hb_direction_t) direction;
  props.script = script;
  props.language = language;
  hb_aat_map_builder_t builder (face, props);
  for (unsigned i = 0; i < num_features; i++)
    builder.add_feature (features[i]);
  hb_aat_map_t map;
  builder.compile (map);
  unsigned size = 1 + map.chain_flags.length;
  for (unsigned i = 0; i < map.chain_flags.length; i++)
    size += 1 + 3 * map.chain_flags[i].length;
  unsigned written = 0;
  if (out && capacity)
  {
    out[written++] = map.chain_flags.length;
    for (unsigned i = 0; i < map.chain_flags.length; i++)
    {
      auto &ranges = map.chain_flags[i];
      if (written >= capacity) break;
      out[written++] = ranges.length;
      for (unsigned r = 0; r < ranges.length; r++)
      {
        if (written + 3 > capacity) break;
        out[written++] = ranges[r].flags;
        out[written++] = ranges[r].cluster_first;
        out[written++] = ranges[r].cluster_last;
      }
    }
  }
  return size;
}

FARFBUZZ_ORACLE_EXPORT void *farfbuzz_oracle_shape_plan_create_cached2 (hb_face_t *face, unsigned direction, hb_script_t script, hb_language_t language, const hb_feature_t *features, unsigned num_features, const int *coords, unsigned num_coords, const char * const *shaper_list);

/* Creates (or returns the cached) shape plan for a face, segment properties, user features,
 * variation coordinates and shaper list, then drops the caller's reference: the plan stays
 * alive through the face's per-face cache, so the returned pointer identifies the plan. */
FARFBUZZ_ORACLE_EXPORT void *
farfbuzz_oracle_shape_plan_create_cached2 (hb_face_t *face, unsigned direction, hb_script_t script, hb_language_t language,
                                           const hb_feature_t *features, unsigned num_features,
                                           const int *coords, unsigned num_coords,
                                           const char * const *shaper_list)
{
  hb_segment_properties_t props = HB_SEGMENT_PROPERTIES_DEFAULT;
  props.direction = (hb_direction_t) direction;
  props.script = script;
  props.language = language;
  hb_shape_plan_t *plan = hb_shape_plan_create_cached2 (face, &props, features, num_features,
                                                        coords, num_coords, shaper_list);
  hb_shape_plan_destroy (plan);
  return (void *) plan;
}

/* trak tracking shim (P8.S13). Returns the device-space tracking advance that
 * hb_aat_layout_track would add to the first glyph of every grapheme: the trak
 * table's get_h/get_v_tracking at scale = upem and the given ptem. */
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_trak_tracking (hb_face_t *face, unsigned horizontal, float ptem);

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_trak_tracking (hb_face_t *face, unsigned horizontal, float ptem)
{
  hb_font_t *font = hb_font_create (face);
  hb_ot_font_set_funcs (font);
  unsigned upem = hb_face_get_upem (face);
  hb_font_set_scale (font, upem, upem);
  hb_font_set_ptem (font, ptem);
  hb_position_t tracking = horizontal
    ? face->table.trak->get_h_tracking (font, 0.f)
    : face->table.trak->get_v_tracking (font, 0.f);
  hb_font_destroy (font);
  return (int) tracking;
}

FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_has_kerning (hb_face_t *face);
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_has_machine_kerning (hb_face_t *face);
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_has_cross_kerning (hb_face_t *face);

/* The kern presence queries (P5.S13); hb_ot_layout_has_kerning and friends are HB_INTERNAL. */
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_has_kerning (hb_face_t *face)
{
  return hb_ot_layout_has_kerning (face);
}
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_has_machine_kerning (hb_face_t *face)
{
  return hb_ot_layout_has_machine_kerning (face);
}
FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_has_cross_kerning (hb_face_t *face)
{
  return hb_ot_layout_has_cross_kerning (face);
}

/* Sanitizes a morx or mort table from its raw bytes (P8.S06). The blocklist is
 * not part of sanitize and is not applied here. */
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_morx_sanitize (const unsigned char *data, unsigned length, unsigned is_mort, unsigned num_glyphs);

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_morx_sanitize (const unsigned char *data, unsigned length, unsigned is_mort, unsigned num_glyphs)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  if (is_mort)
  {
    auto *t = (const AAT::mort *) data;
    return t->sanitize (&c);
  }
  auto *t = (const AAT::morx *) data;
  return t->sanitize (&c);
}

/* Sanitizes a kerx table from its raw bytes (P8.S10). */
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_kerx_sanitize (const unsigned char *data, unsigned length, unsigned num_glyphs);

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_kerx_sanitize (const unsigned char *data, unsigned length, unsigned num_glyphs)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *t = (const AAT::kerx *) data;
  return t->sanitize (&c);
}

/* Sanitizes a kern table from its raw bytes (P8.S11). */
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_kern_sanitize (const unsigned char *data, unsigned length, unsigned num_glyphs);

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_kern_sanitize (const unsigned char *data, unsigned length, unsigned num_glyphs)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *t = (const OT::kern *) data;
  return t->sanitize (&c);
}

/* Sanitizes an ankr table from its raw bytes (P8.S12). */
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_ankr_sanitize (const unsigned char *data, unsigned length, unsigned num_glyphs);

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_ankr_sanitize (const unsigned char *data, unsigned length, unsigned num_glyphs)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *t = (const AAT::ankr *) data;
  return t->sanitize (&c);
}

/* The anchor an ankr table gives a glyph, as a face loads the table: a table that does not sanitize is the Null table.
 * The two coordinates come back packed, x in the high 16 bits and y in the low 16 (P8.S12). */
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_ankr_anchor (const unsigned char *data, unsigned length, unsigned num_glyphs, unsigned glyph, unsigned index);

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_ankr_anchor (const unsigned char *data, unsigned length, unsigned num_glyphs, unsigned glyph, unsigned index)
{
  hb_sanitize_context_t c ((const char *) data, (const char *) data + length);
  c.start_processing ();
  c.set_num_glyphs (num_glyphs);
  auto *t = (const AAT::ankr *) data;
  if (!t->sanitize (&c))
    t = &Null (AAT::ankr);
  const AAT::Anchor &anchor = t->get_anchor (glyph, index, num_glyphs);
  uint16_t x = (int16_t) anchor.xCoordinate;
  uint16_t y = (int16_t) anchor.yCoordinate;
  return (int) (((uint32_t) x << 16) | (uint32_t) y);
}

/* Sanitizes a kerx table the way a face loads it: from a blob, so that the range a subtable limits the sanitizer to is
 * lifted again for the next one. farfbuzz_oracle_kerx_sanitize works on bare bytes and keeps the first subtable's range
 * for every subtable after it (P8.S12). */
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_kerx_load_sane (const unsigned char *data, unsigned length, unsigned num_glyphs);

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_kerx_load_sane (const unsigned char *data, unsigned length, unsigned num_glyphs)
{
  hb_blob_t *blob = hb_blob_create ((const char *) data, length, HB_MEMORY_MODE_DUPLICATE, nullptr, nullptr);
  hb_sanitize_context_t c;
  c.set_num_glyphs (num_glyphs);
  hb_blob_t *sane = c.sanitize_blob<AAT::kerx> (blob);
  int ok = hb_blob_get_length (sane) > 0;
  hb_blob_destroy (sane);
  return ok;
}

/* The index of a script/direction/GSUB-script's shaper in HB_OT_SHAPERS_IMPLEMENT_SHAPERS
 * order (P6.S01): arabic, default, dumber, hangul, hebrew, indic, khmer, myanmar,
 * myanmar_zawgyi, thai, use. Returns 255 for a shaper outside the list. */
FARFBUZZ_ORACLE_EXPORT unsigned farfbuzz_oracle_shaper_categorize (uint32_t script, unsigned direction, uint32_t gsub_script);

FARFBUZZ_ORACLE_EXPORT unsigned
farfbuzz_oracle_shaper_categorize (uint32_t script, unsigned direction, uint32_t gsub_script)
{
  static const hb_ot_shaper_t *const shapers[] = {
    &_hb_ot_shaper_arabic, &_hb_ot_shaper_default, &_hb_ot_shaper_dumber, &_hb_ot_shaper_hangul,
    &_hb_ot_shaper_hebrew, &_hb_ot_shaper_indic, &_hb_ot_shaper_khmer, &_hb_ot_shaper_myanmar,
    &_hb_ot_shaper_myanmar_zawgyi, &_hb_ot_shaper_thai, &_hb_ot_shaper_use };
  const hb_ot_shaper_t *s = hb_ot_shaper_categorize ((hb_script_t) script, (hb_direction_t) direction, gsub_script);
  for (unsigned i = 0; i < ARRAY_LENGTH (shapers); i++)
    if (shapers[i] == s) return i;
  return 255;
}

/* Lowers the length limit and the operation budget the buffer's next shaping call starts from
 * (P11 budget differentials). hb_buffer_t::enter keeps a limit below the idle default
 * (src/hb-buffer.cc:337-347), so a caller of hb_shape sees the smaller of this and the limit
 * the text length gives; a negative argument leaves that limit as it is. hb_buffer_t::leave
 * restores the defaults after the shaping call. */
FARFBUZZ_ORACLE_EXPORT void farfbuzz_oracle_buffer_set_limits (hb_buffer_t *b, int max_len, int max_ops);

FARFBUZZ_ORACLE_EXPORT void
farfbuzz_oracle_buffer_set_limits (hb_buffer_t *b, int max_len, int max_ops)
{
  if (max_len >= 0)
    b->max_len = (unsigned) max_len;
  if (max_ops >= 0)
    b->max_ops = max_ops;
}

/* Timed shaping, stage by stage (performance profile of the general route).
 *
 * farfbuzz_oracle_time_stages shapes every line of a text `rounds` times and reports the time of each stage:
 *   mode 0  the direct stages: hb_ot_shape_internal re-run here piece by piece with a monotonic timestamp between the
 *           pieces (no message callback; every piece is the function upstream calls, in upstream's order);
 *   mode 1  the message stages: a message callback takes a timestamp at every trace message ("start lookup N", "end
 *           table GSUB"...) and the interval between two messages is attributed by the messages that bound it;
 *   mode 2  the untraced shape: hb_shape_plan_execute with no callback, the reference total;
 *   mode 3  a message callback that only returns true (what tracing costs without the timestamp);
 *   mode 4  a message callback that takes a timestamp and returns (what the timestamps add);
 *   mode 5  the untraced shape of a line from the text: the buffer cleared and filled, the segment properties guessed, the plan taken
 *           from the face's cache and executed (what a caller of hb_shape pays per line).
 * The line is added to the buffer outside the timed region; the plan comes from the face's cache outside it too.
 * Returns the number of doubles written to `out`, or -1 when the face does not take the OpenType shaper.
 * out[0] glyphs of one pass, out[1] timer cost in ns (two back-to-back timestamps), out[2] trace messages of one pass,
 * out[3] lines whose direct replica differs from hb_shape_plan_execute (modes 0 only), out[4] total ns of the best
 * round, then for each stage of the mode: ns of the best round, intervals (messages) of that round. */
FARFBUZZ_ORACLE_EXPORT int farfbuzz_oracle_time_stages (const unsigned char *font_data, unsigned font_length,
  const unsigned char *text, unsigned text_length, const int *starts, const int *lengths, unsigned lines,
  unsigned rounds, unsigned mode, double *out, unsigned capacity);
FARFBUZZ_ORACLE_EXPORT const char *farfbuzz_oracle_stage_name (unsigned mode, unsigned index);

#if defined(_WIN32)
static inline uint64_t farfbuzz_now (void)
{
  LARGE_INTEGER counter, frequency;
  QueryPerformanceCounter (&counter);
  QueryPerformanceFrequency (&frequency);
  return (uint64_t) (counter.QuadPart * 1000000000.0 / frequency.QuadPart);
}
#elif defined(__APPLE__)
static inline uint64_t farfbuzz_now (void)
{
  return clock_gettime_nsec_np (CLOCK_UPTIME_RAW);
}
#else
static inline uint64_t farfbuzz_now (void)
{
  struct timespec ts;
  clock_gettime (CLOCK_MONOTONIC, &ts);
  return (uint64_t) ts.tv_sec * 1000000000ull + (uint64_t) ts.tv_nsec;
}
#endif

enum farfbuzz_direct_stage_t
{
  FZD_PROPS, FZD_CIRCLE, FZD_CLUSTERS, FZD_NATIVE, FZD_PREPROCESS, FZD_ROTATE, FZD_NORMALISE, FZD_MASKS, FZD_RECATEGORISE,
  FZD_MAP_GLYPHS, FZD_GLYPH_PROPS, FZD_CLASSES, FZD_GSUB, FZD_ADVANCES, FZD_POSITION_START, FZD_ZERO_EARLY, FZD_GPOS,
  FZD_ZERO_LATE, FZD_FINISH, FZD_FALLBACK_MARKS, FZD_REVERSE, FZD_POST, FZD_PROPAGATE, FZD_GSUB_LOOKUPS, FZD_GSUB_PAUSES, FZD_COUNT
};

static const char *const farfbuzz_direct_names[FZD_COUNT] = {
  "unicode props", "dotted circle", "form clusters", "native direction", "preprocess hook", "rotate chars", "normalise",
  "setup masks", "recategorise marks", "map glyphs", "glyph props (GDEF)", "synthesise classes", "GSUB (whole table)",
  "default advances", "position start", "zero marks (early)", "GPOS (whole table)", "zero marks (late)",
  "finish advances, zero ignorables, finish offsets", "fallback marks", "reverse", "post hook (ignorables, selectors)",
  "propagate flags", "GSUB lookups (inside the GSUB row)", "GSUB pause functions and digest refresh (inside the GSUB row)" };

enum farfbuzz_message_stage_t
{
  FZM_BEFORE_GSUB, FZM_PREPROCESS, FZM_DECOMPOSE, FZM_REORDER, FZM_COMPOSE, FZM_NORMALISER_GAPS, FZM_MASKS_TO_GSUB,
  FZM_GSUB_APPLIED, FZM_GSUB_SKIPPED, FZM_GSUB_HOOKS, FZM_GSUB_GAPS, FZM_BETWEEN_TABLES, FZM_GPOS_APPLIED,
  FZM_GPOS_SKIPPED, FZM_GPOS_HOOKS, FZM_GPOS_GAPS, FZM_KERN, FZM_TAIL, FZM_POSTPROCESS, FZM_COUNT
};

static const char *const farfbuzz_message_names[FZM_COUNT] = {
  "before the first message (props to normaliser, when no hook message)", "preprocess hook", "decompose round",
  "reorder round", "compose round", "between normaliser rounds", "end of normaliser to start table GSUB (masks, glyphs, classes)",
  "GSUB lookups applied", "GSUB lookups skipped (digest)", "GSUB shaper hooks", "GSUB gaps (loop, setup of syllables, digest)",
  "between GSUB and GPOS (advances, early zeroing)", "GPOS lookups applied", "GPOS lookups skipped (digest)", "GPOS hooks",
  "GPOS gaps", "kern table", "after the last table (late zeroing, finishing, reverse, post, propagate)", "postprocess hook" };

FARFBUZZ_ORACLE_EXPORT const char *
farfbuzz_oracle_stage_name (unsigned mode, unsigned index)
{
  if (mode == 0) return index < FZD_COUNT ? farfbuzz_direct_names[index] : nullptr;
  if (mode == 1) return index < FZM_COUNT ? farfbuzz_message_names[index] : nullptr;
  return nullptr;
}

template <typename Tag, typename Tag::type Member>
struct farfbuzz_reach { friend typename Tag::type farfbuzz_reach_get (Tag) { return Member; } };
struct farfbuzz_stages_tag
{
  typedef hb_vector_t<hb_ot_map_t::stage_map_t> (hb_ot_map_t::*type)[2];
  friend type farfbuzz_reach_get (farfbuzz_stages_tag);
};
struct farfbuzz_lookups_tag
{
  typedef hb_vector_t<hb_ot_map_t::lookup_map_t> (hb_ot_map_t::*type)[2];
  friend type farfbuzz_reach_get (farfbuzz_lookups_tag);
};
template struct farfbuzz_reach<farfbuzz_stages_tag, &hb_ot_map_t::stages>;
template struct farfbuzz_reach<farfbuzz_lookups_tag, &hb_ot_map_t::lookups>;

/* hb_ot_map_t::apply for GSUB (hb-ot-layout.cc), loop for loop, with a timestamp before and after each pause function:
 * the time of the lookups and the time of the pauses, directly. */
static void
farfbuzz_substitute_split (const hb_ot_shape_plan_t *plan, hb_font_t *font, hb_buffer_t *buffer, uint64_t *acc, double *counts)
{
  const hb_ot_map_t &map = plan->map;
  GSUBProxy proxy (font->face);
  const unsigned int table_index = proxy.table_index;
  const hb_vector_t<hb_ot_map_t::stage_map_t> &stages = (map.*farfbuzz_reach_get (farfbuzz_stages_tag ()))[table_index];
  const hb_vector_t<hb_ot_map_t::lookup_map_t> &lookups = (map.*farfbuzz_reach_get (farfbuzz_lookups_tag ()))[table_index];
  unsigned int i = 0;
  auto *font_data = font->data.ot.get ();
  auto *var_store_cache = (OT::hb_scalar_cache_t *) font_data;
  OT::hb_ot_apply_context_t c (table_index, font, buffer, proxy.accel.get_blob (), var_store_cache);
  c.set_recurse_func (GSUBProxy::Lookup::template dispatch_recurse_func<OT::hb_ot_apply_context_t>);
  uint64_t last = farfbuzz_now ();
  for (unsigned int stage_index = 0; stage_index < stages.length; stage_index++)
  {
    const hb_ot_map_t::stage_map_t *stage = &stages[stage_index];
    for (; i < stage->last_lookup; i++)
    {
      const hb_ot_map_t::lookup_map_t &lookup = lookups[i];
      unsigned int lookup_index = lookup.index;
      auto *accel = proxy.accel.get_accel (lookup_index);
      if (unlikely (!accel)) continue;
      if (accel->digest.may_intersect (buffer->digest) &&
          ((buffer->flags & HB_BUFFER_FLAG_PRODUCE_UNSAFE_TO_CONCAT) ||
           accel->digest_second.may_intersect (buffer->digest)))
      {
        c.set_lookup_index (lookup_index);
        c.set_lookup_mask (lookup.mask, false);
        c.set_auto_zwj (lookup.auto_zwj, false);
        c.set_auto_zwnj (lookup.auto_zwnj, false);
        c.set_random (lookup.random);
        c.set_per_syllable (lookup.per_syllable, false);
        apply_string<GSUBProxy> (&c, proxy.accel.table->get_lookup (lookup_index), *accel);
      }
    }
    if (stage->pause_func)
    {
      uint64_t before = farfbuzz_now ();
      acc[FZD_GSUB_LOOKUPS] += before - last;
      counts[FZD_GSUB_LOOKUPS] += 1;
      if (stage->pause_func (plan, font, buffer))
        buffer->update_digest ();
      last = farfbuzz_now ();
      acc[FZD_GSUB_PAUSES] += last - before;
      counts[FZD_GSUB_PAUSES] += 1;
    }
  }
  uint64_t end = farfbuzz_now ();
  acc[FZD_GSUB_LOOKUPS] += end - last;
  counts[FZD_GSUB_LOOKUPS] += 1;
}

static void
farfbuzz_shape_direct (hb_ot_shape_context_t *c, uint64_t *acc, double *counts)
{
  uint64_t last = farfbuzz_now ();
#define FZ_MARK(s) do { uint64_t n_ = farfbuzz_now (); acc[s] += n_ - last; last = n_; } while (0)
  hb_buffer_t *buffer = c->buffer;
  c->target_direction = buffer->props.direction;
  _hb_buffer_allocate_unicode_vars (buffer);
  last = farfbuzz_now ();
  hb_set_unicode_props (buffer, c->plan->map.get_global_mask ());
  FZ_MARK (FZD_PROPS);
  hb_insert_dotted_circle (buffer, c->font);
  FZ_MARK (FZD_CIRCLE);
  hb_form_clusters (buffer);
  FZ_MARK (FZD_CLUSTERS);
  hb_ensure_native_direction (buffer);
  FZ_MARK (FZD_NATIVE);
  if (c->plan->shaper->preprocess_text)
    c->plan->shaper->preprocess_text (c->plan, buffer, c->font);
  FZ_MARK (FZD_PREPROCESS);

  hb_ot_rotate_chars (c);
  FZ_MARK (FZD_ROTATE);
  HB_BUFFER_ALLOCATE_VAR (buffer, normalizer_glyph_index);
  _hb_ot_shape_normalize (c->plan, buffer, c->font);
  FZ_MARK (FZD_NORMALISE);
  hb_ot_shape_setup_masks (c);
  FZ_MARK (FZD_MASKS);
  if (c->plan->fallback_mark_positioning)
    _hb_ot_shape_fallback_mark_position_recategorize_marks (c->plan, c->font, buffer);
  FZ_MARK (FZD_RECATEGORISE);
  hb_ot_map_glyphs_fast (buffer);
  HB_BUFFER_DEALLOCATE_VAR (buffer, normalizer_glyph_index);
  FZ_MARK (FZD_MAP_GLYPHS);
  _hb_buffer_allocate_gsubgpos_vars (buffer);
  hb_ot_layout_substitute_start_with_digest (c->font, buffer);
  FZ_MARK (FZD_GLYPH_PROPS);
  if (c->plan->fallback_glyph_classes)
    hb_synthesize_glyph_classes (buffer);
  FZ_MARK (FZD_CLASSES);
  farfbuzz_substitute_split (c->plan, c->font, buffer, acc, counts);
  FZ_MARK (FZD_GSUB);

  buffer->clear_positions ();
  hb_ot_position_default (c);
  FZ_MARK (FZD_ADVANCES);
  bool adjust_offsets_when_zeroing = c->plan->adjust_mark_positioning_when_zeroing &&
                                     HB_DIRECTION_IS_FORWARD (buffer->props.direction);
  if (c->font->has_glyph_h_origin_func () || c->font->has_glyph_h_origins_func ())
    c->font->add_glyph_h_origins (buffer);
  hb_ot_layout_position_start (c->font, buffer);
  FZ_MARK (FZD_POSITION_START);
  if (c->plan->zero_marks && c->plan->shaper->zero_width_marks == HB_OT_SHAPE_ZERO_WIDTH_MARKS_BY_GDEF_EARLY)
    zero_mark_widths_by_gdef (buffer, adjust_offsets_when_zeroing);
  FZ_MARK (FZD_ZERO_EARLY);
  c->plan->position (c->font, buffer);
  FZ_MARK (FZD_GPOS);
  if (c->plan->zero_marks && c->plan->shaper->zero_width_marks == HB_OT_SHAPE_ZERO_WIDTH_MARKS_BY_GDEF_LATE)
    zero_mark_widths_by_gdef (buffer, adjust_offsets_when_zeroing);
  FZ_MARK (FZD_ZERO_LATE);
  hb_ot_layout_position_finish_advances (c->font, buffer);
  hb_ot_zero_width_default_ignorables (buffer);
  hb_ot_layout_position_finish_offsets (c->font, buffer);
  if (c->font->has_glyph_h_origin_func () || c->font->has_glyph_h_origins_func ())
    c->font->subtract_glyph_h_origins (buffer);
  FZ_MARK (FZD_FINISH);
  if (c->plan->fallback_mark_positioning)
    _hb_ot_shape_fallback_mark_position (c->plan, c->font, buffer, adjust_offsets_when_zeroing);
  FZ_MARK (FZD_FALLBACK_MARKS);
  if (HB_DIRECTION_IS_BACKWARD (buffer->props.direction))
    hb_buffer_reverse (buffer);
  _hb_buffer_deallocate_gsubgpos_vars (buffer);
  FZ_MARK (FZD_REVERSE);
  hb_ot_deal_with_variation_selectors (buffer);
  hb_ot_hide_default_ignorables (buffer, c->font);
  if (c->plan->shaper->postprocess_glyphs)
    c->plan->shaper->postprocess_glyphs (c->plan, buffer, c->font);
  FZ_MARK (FZD_POST);
  hb_propagate_flags (buffer);
  _hb_buffer_deallocate_unicode_vars (buffer);
  buffer->props.direction = c->target_direction;
  FZ_MARK (FZD_PROPAGATE);
#undef FZ_MARK
}

enum farfbuzz_event_kind_t
{
  FZE_START, FZE_START_GSUB, FZE_END_GSUB, FZE_START_GPOS, FZE_END_GPOS, FZE_START_KERN, FZE_END_KERN,
  FZE_START_LOOKUP, FZE_END_LOOKUP, FZE_SKIPPED, FZE_START_PREPROCESS, FZE_END_PREPROCESS, FZE_START_DECOMPOSE,
  FZE_END_DECOMPOSE, FZE_START_REORDER, FZE_END_REORDER, FZE_START_COMPOSE, FZE_END_COMPOSE, FZE_START_HOOK,
  FZE_END_HOOK, FZE_START_POST, FZE_END_POST, FZE_OTHER
};

struct farfbuzz_event_t { uint8_t kind; uint64_t time; };

struct farfbuzz_recorder_t
{
  farfbuzz_event_t events[1 << 16];
  unsigned count;
};

static farfbuzz_recorder_t farfbuzz_recorder;

static unsigned
farfbuzz_classify (const char *m)
{
  bool start = !strncmp (m, "start ", 6);
  bool end = !strncmp (m, "end ", 4);
  bool skipped = !strncmp (m, "skipped ", 8);
  const char *w = m + (start ? 6 : end ? 4 : skipped ? 8 : 0);
  if (skipped) return !strncmp (w, "lookup", 6) ? FZE_SKIPPED : FZE_OTHER;
  if (!start && !end) return FZE_OTHER;
  if (!strncmp (w, "table GSUB", 10)) return start ? FZE_START_GSUB : FZE_END_GSUB;
  if (!strncmp (w, "table GPOS", 10)) return start ? FZE_START_GPOS : FZE_END_GPOS;
  if (!strncmp (w, "table ", 6)) return start ? FZE_START_KERN : FZE_END_KERN;
  if (!strncmp (w, "lookup", 6)) return start ? FZE_START_LOOKUP : FZE_END_LOOKUP;
  if (!strncmp (w, "preprocess-text", 15)) return start ? FZE_START_PREPROCESS : FZE_END_PREPROCESS;
  if (!strncmp (w, "decompose", 9)) return start ? FZE_START_DECOMPOSE : FZE_END_DECOMPOSE;
  if (!strncmp (w, "reorder", 7) && w[7] == 0) return start ? FZE_START_REORDER : FZE_END_REORDER;
  if (!strncmp (w, "compose", 7)) return start ? FZE_START_COMPOSE : FZE_END_COMPOSE;
  if (!strncmp (w, "postprocess-glyphs", 18)) return start ? FZE_START_POST : FZE_END_POST;
  return start ? FZE_START_HOOK : FZE_END_HOOK;
}

static hb_bool_t
farfbuzz_message_record (hb_buffer_t *, hb_font_t *, const char *message, void *)
{
  farfbuzz_recorder_t &r = farfbuzz_recorder;
  if (r.count < (1u << 16))
  {
    r.events[r.count].time = farfbuzz_now ();
    r.events[r.count].kind = (uint8_t) farfbuzz_classify (message);
    r.count++;
  }
  return true;
}

static hb_bool_t
farfbuzz_message_stamp (hb_buffer_t *, hb_font_t *, const char *, void *user)
{
  *(uint64_t *) user = farfbuzz_now ();
  return true;
}

static hb_bool_t
farfbuzz_message_noop (hb_buffer_t *, hb_font_t *, const char *, void *)
{
  return true;
}

static void
farfbuzz_attribute (uint64_t start, uint64_t end, double *acc, double *intervals)
{
  farfbuzz_recorder_t &r = farfbuzz_recorder;
  unsigned table = 0;
  unsigned previous = FZE_START;
  bool normalising = false;
  uint64_t before = start;
  for (unsigned i = 0; i <= r.count; i++)
  {
    unsigned kind = i < r.count ? r.events[i].kind : FZE_START;
    uint64_t now = i < r.count ? r.events[i].time : end;
    double dt = (double) (now - before);
    unsigned category;
    bool skippedNext = (previous == FZE_START_LOOKUP && kind == FZE_SKIPPED) || previous == FZE_SKIPPED;
    switch (previous)
    {
      case FZE_START: category = FZM_BEFORE_GSUB; break;
      case FZE_START_PREPROCESS: category = FZM_PREPROCESS; break;
      case FZE_START_DECOMPOSE: category = FZM_DECOMPOSE; break;
      case FZE_START_REORDER: category = FZM_REORDER; break;
      case FZE_START_COMPOSE: category = FZM_COMPOSE; break;
      case FZE_START_POST: category = FZM_POSTPROCESS; break;
      case FZE_START_LOOKUP:
      case FZE_SKIPPED:
        category = table == 1 ? (skippedNext ? FZM_GSUB_SKIPPED : FZM_GSUB_APPLIED) : (skippedNext ? FZM_GPOS_SKIPPED : FZM_GPOS_APPLIED);
        break;
      case FZE_START_HOOK: category = table == 1 ? FZM_GSUB_HOOKS : FZM_GPOS_HOOKS; break;
      case FZE_END_PREPROCESS: category = FZM_BEFORE_GSUB; break;
      case FZE_END_DECOMPOSE:
      case FZE_END_REORDER:
      case FZE_END_COMPOSE:
        category = kind == FZE_START_GSUB ? FZM_MASKS_TO_GSUB : FZM_NORMALISER_GAPS;
        break;
      case FZE_START_GSUB:
      case FZE_END_LOOKUP:
      case FZE_END_HOOK:
        category = table == 1 ? FZM_GSUB_GAPS : FZM_GPOS_GAPS;
        break;
      case FZE_END_GSUB: category = FZM_BETWEEN_TABLES; break;
      case FZE_START_GPOS: category = FZM_GPOS_GAPS; break;
      case FZE_START_KERN: category = FZM_KERN; break;
      case FZE_END_GPOS:
      case FZE_END_KERN:
      case FZE_END_POST:
      case FZE_OTHER:
        category = FZM_TAIL;
        break;
      default: category = FZM_TAIL; break;
    }
    if (previous == FZE_START && kind == FZE_START_GSUB) category = FZM_MASKS_TO_GSUB;
    acc[category] += dt;
    intervals[category] += 1;
    before = now;
    previous = kind;
    if (kind == FZE_START_GSUB) table = 1;
    else if (kind == FZE_START_GPOS) table = 2;
    (void) normalising;
  }
}

FARFBUZZ_ORACLE_EXPORT int
farfbuzz_oracle_time_stages (const unsigned char *font_data, unsigned font_length,
  const unsigned char *text, unsigned text_length, const int *starts, const int *lengths, unsigned lines,
  unsigned rounds, unsigned mode, double *out, unsigned capacity)
{
  const unsigned maxStages = FZM_COUNT > FZD_COUNT ? FZM_COUNT : FZD_COUNT;
  unsigned stages = mode == 0 ? FZD_COUNT : mode == 1 ? FZM_COUNT : 0;
  if (capacity < 5 + 2 * stages) return -1;
  hb_blob_t *blob = hb_blob_create ((const char *) font_data, font_length, HB_MEMORY_MODE_READONLY, nullptr, nullptr);
  hb_face_t *face = hb_face_create (blob, 0);
  hb_font_t *font = hb_font_create (face);
  hb_ot_font_set_funcs (font);
  hb_buffer_t *buffer = hb_buffer_create ();

  double timer = 1e30;
  for (int batch = 0; batch < 200; batch++)
  {
    uint64_t sum = 0;
    for (int i = 0; i < 500; i++)
    {
      uint64_t a = farfbuzz_now ();
      uint64_t b = farfbuzz_now ();
      sum += b - a;
    }
    if ((double) sum / 500.0 < timer) timer = (double) sum / 500.0;
  }

  double *lineBest = new double[lines];
  double *lineStage = new double[(size_t) lines * maxStages];
  double *lineIntervals = new double[(size_t) lines * maxStages];
  double glyphs = 0, messages = 0, differing = 0;
  uint64_t stamp = 0;
  for (unsigned line = 0; line < lines; line++) lineBest[line] = 1e30;

  for (unsigned round = 0; round <= rounds; round++)
  {
    for (unsigned line = 0; line < lines; line++)
    {
      double stageAcc[FZM_COUNT > FZD_COUNT ? FZM_COUNT : FZD_COUNT] = {0};
      double intervalAcc[FZM_COUNT > FZD_COUNT ? FZM_COUNT : FZD_COUNT] = {0};
      uint64_t directAcc[FZD_COUNT] = {0};
      double total = 0;
      if (mode == 5)
      {
        uint64_t a = farfbuzz_now ();
        hb_buffer_clear_contents (buffer);
        hb_buffer_add_utf8 (buffer, (const char *) text + starts[line], (int) (text_length - starts[line]), 0, lengths[line]);
        hb_buffer_guess_segment_properties (buffer);
        hb_shape_plan_t *whole = hb_shape_plan_create_cached2 (face, &buffer->props, nullptr, 0, nullptr, 0, nullptr);
        hb_shape_plan_execute (whole, font, buffer, nullptr, 0);
        hb_shape_plan_destroy (whole);
        total = (double) (farfbuzz_now () - a);
        if (round == 0) glyphs += hb_buffer_get_length (buffer);
        if (round > 0 && total < lineBest[line]) lineBest[line] = total;
        continue;
      }
      hb_buffer_clear_contents (buffer);
      hb_buffer_add_utf8 (buffer, (const char *) text + starts[line], (int) (text_length - starts[line]), 0, lengths[line]);
      hb_buffer_guess_segment_properties (buffer);
      if (!buffer->len) continue;
      hb_shape_plan_t *plan = hb_shape_plan_create_cached2 (face, &buffer->props, nullptr, 0, nullptr, 0, nullptr);
      if (mode == 0)
      {
        hb_ot_shape_context_t c = {&plan->ot, font, face, buffer, nullptr, 0};
        if (plan->ot.apply_morx) { hb_shape_plan_destroy (plan); return -1; }
        buffer->enter ();
        uint64_t a = farfbuzz_now ();
        farfbuzz_shape_direct (&c, directAcc, intervalAcc);
        total = (double) (farfbuzz_now () - a);
        buffer->leave ();
        buffer->content_type = HB_BUFFER_CONTENT_TYPE_GLYPHS;
        if (round == 0)
        {
          unsigned len = hb_buffer_get_length (buffer);
          hb_glyph_info_t *info = hb_buffer_get_glyph_infos (buffer, nullptr);
          hb_glyph_position_t *pos = hb_buffer_get_glyph_positions (buffer, nullptr);
          hb_buffer_t *check = hb_buffer_create ();
          hb_buffer_add_utf8 (check, (const char *) text + starts[line], (int) (text_length - starts[line]), 0, lengths[line]);
          hb_buffer_guess_segment_properties (check);
          hb_shape_plan_execute (plan, font, check, nullptr, 0);
          bool same = hb_buffer_get_length (check) == len;
          hb_glyph_info_t *ci = hb_buffer_get_glyph_infos (check, nullptr);
          hb_glyph_position_t *cp = hb_buffer_get_glyph_positions (check, nullptr);
          for (unsigned i = 0; same && i < len; i++)
            same = ci[i].codepoint == info[i].codepoint && ci[i].cluster == info[i].cluster &&
                   cp[i].x_advance == pos[i].x_advance && cp[i].y_advance == pos[i].y_advance &&
                   cp[i].x_offset == pos[i].x_offset && cp[i].y_offset == pos[i].y_offset;
          if (!same) differing += 1;
          hb_buffer_destroy (check);
          glyphs += len;
        }
      }
      else
      {
        if (mode == 1)
        {
          farfbuzz_recorder.count = 0;
          hb_buffer_set_message_func (buffer, farfbuzz_message_record, nullptr, nullptr);
        }
        else if (mode == 3)
          hb_buffer_set_message_func (buffer, farfbuzz_message_noop, nullptr, nullptr);
        else if (mode == 4)
          hb_buffer_set_message_func (buffer, farfbuzz_message_stamp, &stamp, nullptr);
        uint64_t a = farfbuzz_now ();
        hb_shape_plan_execute (plan, font, buffer, nullptr, 0);
        uint64_t b = farfbuzz_now ();
        total = (double) (b - a);
        if (mode == 1)
        {
          farfbuzz_attribute (a, b, stageAcc, intervalAcc);
          if (round == 0) messages += farfbuzz_recorder.count;
        }
        if (mode != 2)
          hb_buffer_set_message_func (buffer, nullptr, nullptr, nullptr);
        if (round == 0) glyphs += hb_buffer_get_length (buffer);
      }
      hb_shape_plan_destroy (plan);
      if (round > 0 && total < lineBest[line])
      {
        lineBest[line] = total;
        for (unsigned s = 0; s < stages; s++)
        {
          lineStage[(size_t) line * maxStages + s] = mode == 0 ? (double) directAcc[s] : stageAcc[s];
          lineIntervals[(size_t) line * maxStages + s] = intervalAcc[s];
        }
      }
    }
  }
  double best = 0;
  double stageSum[FZM_COUNT > FZD_COUNT ? FZM_COUNT : FZD_COUNT] = {0};
  double intervalSum[FZM_COUNT > FZD_COUNT ? FZM_COUNT : FZD_COUNT] = {0};
  for (unsigned line = 0; line < lines; line++)
  {
    if (lineBest[line] > 1e29) continue;
    best += lineBest[line];
    for (unsigned s = 0; s < stages; s++)
    {
      stageSum[s] += lineStage[(size_t) line * maxStages + s];
      intervalSum[s] += lineIntervals[(size_t) line * maxStages + s];
    }
  }
  out[0] = glyphs;
  out[1] = timer;
  out[2] = messages;
  out[3] = differing;
  out[4] = best;
  for (unsigned s = 0; s < stages; s++)
  {
    out[5 + 2 * s] = stageSum[s];
    out[6 + 2 * s] = intervalSum[s];
  }
  delete[] lineBest;
  delete[] lineStage;
  delete[] lineIntervals;
  hb_buffer_destroy (buffer);
  hb_font_destroy (font);
  hb_face_destroy (face);
  hb_blob_destroy (blob);
  return (int) (5 + 2 * stages);
}
