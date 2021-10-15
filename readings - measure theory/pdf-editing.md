# Use `qpdf` to edit a PDF to have page numbers correct.

[Here's how.](https://superuser.com/a/1367604/1276377)

What I did:

1. Install `qpdf`.
2. Convert to qdf format running `qpdf -qdf foo.pdf TEMP.qdf`
3. Edit the TEMP.qdf file. I added the commented lines:

  ```
  1 0 obj
  <<
  /Metadata 3 0 R
  /Outlines 5 0 R
  /Pages 6 0 R
  /Type /Catalog
    /PageLabels <<        % Added 'PageLabels' in the Catalog
      /Nums [             % 'Nums' for the page numbers
        0 << /P (cover) >>% Cover page is labeled as 'cover'
        1 << /S /r >>     % Lowecase roman numerals on first pages.
        17 << /S /D >>    % Start numbering from 18th page.
      ]
    >>
  >>
  endobj
  ```
4. Run `fix-qdf TEMP.qdf > TEMP-fixed.qdf` to fix possible errors
5. Convert back to PDF format `qpdf TEMP-fixed.qdf foo-edited.pdf`
6. Clean `rm TEMP.qdf TEMP-fixed.qdf`
