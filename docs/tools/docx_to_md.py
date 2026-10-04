# docx_to_md.py -- regenerate docs/Democare_2026.md from "Democare 2026.docx"
#
# Usage (from the repository root, with pandoc and Python 3 installed):
#     python3 docs/tools/docx_to_md.py "path/to/Democare 2026.docx"
#
# What it does:
#   1. accepts any tracked changes and drops comment markers (XML level, so
#      the Word equations are kept; LibreOffice would drop them);
#   2. splits Word field runs so that pandoc reads the figure/table numbers;
#   3. converts with pandoc to GitHub Markdown, extracting the images to
#      docs/media/;
#   4. tidies the result: new table of contents, table captions put back
#      above their tables, code blocks with their indentation, image sizes,
#      display equations inside tables turned into inline equations.
# Figure 9 is a Word chart, which pandoc cannot convert: the image
# docs/media/figure9_chart.png is kept as it is and must be replaced by
# hand if the chart changes.

import html, os, re, shutil, subprocess, sys, tempfile, zipfile

DOCX = sys.argv[1]
DOCS = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..')
MD = os.path.join(DOCS, 'Democare_2026.md')


def accept_and_split(src, dst_dir):
    with zipfile.ZipFile(src) as z:
        z.extractall(dst_dir)
    p = os.path.join(dst_dir, 'word', 'document.xml')
    x = open(p, encoding='utf8').read()
    x = re.sub(r'<w:del [^>]*>.*?</w:del>', '', x, flags=re.S)
    x = re.sub(r'<w:ins [^>]*/>', '', x)
    x = re.sub(r'<w:ins [^>]*>', '', x).replace('</w:ins>', '')
    x = re.sub(r'<w:commentRange(Start|End) [^>]*/>', '', x)
    x = re.sub(r'<w:r>(<w:rPr>(?:(?!</w:rPr>).)*</w:rPr>)?<w:commentReference [^>]*/></w:r>', '', x, flags=re.S)

    def split(m):
        r = m.group(0)
        if 'w:fldChar' not in r and 'w:instrText' not in r:
            return r
        mm = re.match(r'<w:r( [^>]*)?>(<w:rPr>.*?</w:rPr>)?(.*)</w:r>$', r, flags=re.S)
        attrs, rpr, inner = mm.group(1) or '', mm.group(2) or '', mm.group(3)
        kids = re.findall(r'<w:(?:t|instrText)(?: [^>]*)?>.*?</w:(?:t|instrText)>|<[^/][^>]*/>', inner, flags=re.S)
        return ''.join('<w:r%s>%s%s</w:r>' % (attrs, rpr, k) for k in kids)
    x = re.sub(r'<w:r(?: [^>]*)?>.*?</w:r>', split, x, flags=re.S)
    open(p, 'w', encoding='utf8').write(x)


def main():
    tmp = tempfile.mkdtemp()
    WORK = os.path.join(tmp, 'docx')
    accept_and_split(DOCX, WORK)
    clean = os.path.join(tmp, 'clean.docx')
    with zipfile.ZipFile(clean, 'w', zipfile.ZIP_DEFLATED) as z:
        for root, _, files in os.walk(WORK):
            for f in files:
                full = os.path.join(root, f)
                z.write(full, os.path.relpath(full, WORK))
    chart = os.path.join(DOCS, 'media', 'figure9_chart.png')
    keep = open(chart, 'rb').read() if os.path.exists(chart) else None
    subprocess.run(['pandoc', clean, '-t', 'gfm', '--wrap=none', '--extract-media=.',
                    '-o', 'Democare_2026.md'], cwd=DOCS, check=True)
    if keep:
        open(chart, 'wb').write(keep)


    # ---- code paragraphs from the docx (Courier New), to restore indentation
    x = open(os.path.join(WORK, 'word', 'document.xml'), encoding='utf8').read()
    body = x[x.index('<w:body>'):]
    code = []
    for p in re.findall(r'<w:p[ >].*?</w:p>', body, flags=re.S):
        runs = [r for r in re.findall(r'<w:r(?: [^>]*)?>.*?</w:r>', p, flags=re.S) if re.search(r'<w:t[ >]|<w:br/>', r)]
        if runs and all('Courier' in r for r in runs):
            t = ''
            for r in runs:
                for m in re.finditer(r'<w:t(?: [^>]*)?>([^<]*)</w:t>|<w:br/>', r):
                    t += '\n' if m.group(0) == '<w:br/>' else html.unescape(m.group(1))
            code.append(t)

    # table captions, in order
    caps = {}
    for p in re.findall(r'<w:p[ >].*?</w:p>', body, flags=re.S):
        if '<w:pStyle w:val="Caption"/>' in p:
            t = html.unescape(''.join(re.findall(r'<w:t(?: [^>]*)?>([^<]*)</w:t>', p)))
            m = re.match(r'Table (\d+)\.', t)
            if m:
                caps[int(m.group(1))] = t

    md = open(MD, encoding='utf8').read()
    lines = md.split('\n')

    # ---- 1. drop the Word table of contents (link lines before "## Introduction")
    start = next(i for i, l in enumerate(lines) if l.startswith('[DEMOCARE: Documentation'))
    end = next(i for i, l in enumerate(lines) if l.startswith('## Introduction'))
    toc_at = start
    lines = lines[:start] + ['@@TOC@@', ''] + lines[end:]
    md = '\n'.join(lines)

    # ---- 2. yellow highlight spans
    md = re.sub(r'<span class="mark">(.*?)</span>', r'\1', md, flags=re.S)

    # ---- 3. images: keep relative size, GitHub ignores style=
    def img(m):
        src, style, alt = m.group(1), m.group(2) or '', m.group(3) or ''
        w = re.search(r'width:([\d.]+)in', style)
        width = min(int(float(w.group(1)) * 96), 800) if w else 600
        src = src.replace('./media/', 'media/')
        return '<img src="%s" width="%d" alt="%s" />' % (src, width, alt)
    md = re.sub(r'<img src="([^"]+)"(?: style="([^"]*)")?(?: alt="([^"]*)")? />', img, md)

    # ---- 4. table captions: pandoc moves them; put each one back above its table
    md = re.sub(r'\n\n(?:> )?Table \d+\.[^\n]*', '', md)
    t1 = md.index('| **Activity status**')
    md = md[:t1] + '**' + caps[1] + '**\n\n' + md[t1:]
    vn = [m.start() for m in re.finditer(r'^\| \*\*Variable name\*\*', md, flags=re.M)]
    assert len(vn) == 2, vn
    for k, pos in zip((3, 2), reversed(vn)):
        md = md[:pos] + '**' + caps[k] + '**\n\n' + md[pos:]

    # ---- 5. figure 10 caption number lost in the field, and the Figure 9 chart
    md = md.replace('\nFigure . Individual data', '\nFigure 10. Individual data')
    f9 = re.search(r'\nFigure 9\. [^\n]*\n', md)
    md = md[:f9.end()] + '\n<img src="media/figure9_chart.png" width="500" alt="Figure 9: proportions of activity A, C, P by age" />\n' + md[f9.end():]

    # ---- 6. code blocks with their original indentation
    def unesc(s):
        return re.sub(r'\\(.)', r'\1', s)

    # NetLogo snippet: five consecutive one-line paragraphs
    net = code[:5]
    first = md.index('if (dice \\< refValueChangeH)')
    last_line = '; else: no transition, stays in current activity'
    stop = md.index(last_line, first) + len(last_line)
    md = md[:first] + '```\n' + '\n'.join(net) + '\n```' + md[stop:]

    for block in code[5:]:
        first_line = block.split('\n')[0]
        cands = [first_line.replace('<', '\\<').replace('#', '\\#'), first_line]
        # >>> Claude 2026-10-03
        # pandoc can also escape underscores and other Markdown characters
        cands.append(first_line.replace('_', '\\_').replace('<', '\\<').replace('#', '\\#'))
        cands.append(re.sub(r'([\\`*_\[\]<>#|~])', r'\\\1', first_line))
        # <<< Claude 2026-10-03
        i = -1
        for c in cands:
            m = re.search(r'\n' + re.escape(c) + r'(  )?\n', md)
            if m:
                i = m.start() + 1
                break
        assert i >= 0, first_line
        j = md.find('\n\n', i)
        # the escaped block can contain blank-looking lines ("  \n"), so end at a line with no trailing hard break
        k = i
        while True:
            nl = md.find('\n', k)
            line = md[k:nl]
            if not line.endswith('  ') and not line.strip() == '':
                j = nl
                break
            k = nl + 1
        md = md[:i] + '```r\n' + block + '\n```' + md[j:]

    # ---- 7. table of contents from the headings (GitHub anchors)
    toc = []
    seen = {}
    for m in re.finditer(r'^(#{2,3}) (.+)$', md, flags=re.M):
        level, title = len(m.group(1)), m.group(2).strip()
        a = re.sub(r'[^\w\- ]', '', title.lower()).replace(' ', '-')
        n = seen.get(a, 0); seen[a] = n + 1
        if n: a += '-%d' % n
        toc.append('%s- [%s](#%s)' % ('  ' * (level - 2), title, a))
    note = ('> This file is generated from *Democare 2026.docx* (with all tracked changes '
            'accepted). The Word file is the master copy: edit the Word file and regenerate this one, rather '
            'than editing this file by hand.')
    md = md.replace('@@TOC@@', note + '\n\n## Contents\n\n' + '\n'.join(toc))

    open(MD, 'w', encoding='utf8').write(md)


    # display equations inside table cells are not rendered by GitHub
    md = re.sub(r'\| \$\$(.+?)\$\$ \|', r'| $\1$ |', md)
    open(MD, 'w', encoding='utf8').write(md)
    shutil.rmtree(tmp)
    print('written', MD)


if __name__ == '__main__':
    main()
