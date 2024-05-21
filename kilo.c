/* libraries */

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/* definitions */

#define KILO_VERSION        "0.0.1"
#define KILO_TAB_STOP       4
#define KILO_QUIT_TIMES     3
#define CTRL_KEY(k)         ((k) & 0x1f)

enum editorKey
{
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

enum editorHighlight
{
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,
    HL_NUMBER,
    HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS    (1 << 0)
#define HL_HIGHLIGHT_STRINGS    (1 << 1)

/* data */

struct editorSyntax
{
    char * file_type;
    char ** file_match;
    char ** keywords;
    char * single_line_comment_start;
    char * multi_line_comment_start;
    char * multi_line_comment_end;
    int flags;
};

typedef struct e_row
{
    int idx;
    int size;
    int r_size;
    char * chars;
    char * render;
    unsigned char * hl;
    int hl_open_comment;
} e_row;

struct editorConfig
{
    int cx, cy;
    int rx;
    int row_off;
    int col_off;
    int screen_rows;
    int screen_cols;
    int num_rows;
    e_row * row;
    int dirty;
    char * file_name;
    char status_msg[80];
    time_t status_msg_time;
    struct editorSyntax * syntax;
    struct termios orig_termios;
};

struct editorConfig E;

/* filetypes */

char * C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case",
  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  "void|", NULL
};

struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
};

#define HLDB_ENTRIES    (sizeof(HLDB) / sizeof(HLDB[0]))

/* prototypes */

void editorSetStatusMessage(const char * fmt, ...);
void editorRefreshScreen();
char * editorPrompt(char * prompt, void (*callback)(char *, int));

/* terminal */

void die(const char * s)
{
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);
    perror(s);
    exit(1);
}

void disableRawMode()
{
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
		die("tcsetattr");
}

void enableRawMode()
{
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
		die("tcgetattr");
    atexit(disableRawMode);

    struct termios raw = E.orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
		die("tcsetattr");
}

int editorReadKey()
{
    int nread;
    char c;

    while ((nread = read(STDIN_FILENO, &c, 1)) != 1)
        if (nread == -1 && errno != EAGAIN)
			die("read");
    
    if (c == '\x1b')
    {
        char seq[3];
        
        if (read(STDIN_FILENO, &seq[0], 1) != 1)
			return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1)
			return '\x1b';

        if (seq[0] == '[')
        {
            if (seq[1] >= '0' && seq[1] <= '9')
            {
                if (read(STDIN_FILENO, &seq[2], 1) != 1)
					return '\x1b';
                
				if (seq[2] == '~')
                {
                    switch (seq[1])
                    {
                        case '1': return HOME_KEY;
                        case '3': return DEL_KEY;
                        case '4': return END_KEY;
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        case '7': return HOME_KEY;
                        case '8': return END_KEY;
                    }
                }
            }
            else
            {
                switch (seq[1])
                {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        }
        else if (seq[0] == '0')
        {
            switch(seq[1])
            {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }

        return '\x1b';
    }
    else
    {
        return c;
    }
}

int getCursorPosition(int * rows, int * cols)
{
    char buf[32];
    unsigned int i = 0;

    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4)
		return -1;

    while (i < sizeof(buf) - 1)
    {
        if (read(STDIN_FILENO, &buf[i], 1) != 1)
			break;
        
		if (buf[i] == 'R')
			break;
        ++i;
    }
    buf[i] = '\0';
    
    if (buf[0] != '\x1b' || buf[1] != '[')
		return -1;
    
	if (sscanf(&buf[2], "%d;%d", rows, cols) != 2)
		return -1;

    return 0;
}

int getWindowSize(int * rows, int * cols)
{
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0)
    {
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12)
			return -1;
        return getCursorPosition(rows, cols);
    }
    else
    {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

/* syntax highlighting */

int isSeparator(int c)
{
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(e_row * row)
{
    row -> hl = realloc(row -> hl, row -> r_size);
    memset(row -> hl, HL_NORMAL, row -> r_size);

    if (E.syntax == NULL) return;

    char ** keywords = E.syntax -> keywords;

    char * scs = E.syntax -> single_line_comment_start;
    char * mcs = E.syntax -> multi_line_comment_start;
    char * mce = E.syntax -> multi_line_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;

    int prev_sep = 1;
    int in_string = 0;
    int in_comment = (row -> idx > 0 && E.row[row -> idx - 1].hl_open_comment);

    int i = 0;
    while (i < row -> r_size)
    {
        char c = row -> render[i];
        unsigned char prev_hl = (i > 0) ? row -> hl[i - 1] : HL_NORMAL;

        if (scs_len && !in_string && !in_comment)
        {
            if (!strncmp(&row -> render[i], scs, scs_len))
            {
                memset(&row -> hl[i], HL_COMMENT, row -> r_size - i);
                break;
            }
        }

        if (mcs_len && mce_len && !in_string)
        {
            if (in_comment)
            {
                row -> hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row -> render[i], mce, mce_len))
                {
                    memset(&row -> hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                }
                else
                {
                    ++i;
                    continue;
                }
            }
            else if (!strncmp(&row -> render[i], mcs, mcs_len))
            {
                memset(&row -> hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }
        
        if (E.syntax -> flags & HL_HIGHLIGHT_STRINGS)
        {
            if (in_string)
            {
                row -> hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row -> r_size)
                {
                    row -> hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                
				}
                if (c == in_string)
					in_string = 0;
                ++i;
                prev_sep = 1;
                continue;
            }
            else
            {
                if (c == '"' || c == '\'')
                {
                    in_string = c;
                    row -> hl[i] = HL_STRING;
                    ++i;
                    continue;
                }
            }
        }
        
        if (E.syntax -> flags & HL_HIGHLIGHT_NUMBERS)
        {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) || (c == '.' && prev_hl == HL_NUMBER))
            {
                row -> hl[i] = HL_NUMBER;
                ++i;
                prev_sep = 0;
                continue;
            }
        }

        if (prev_sep)
        {
            int j;
            for (j = 0; keywords[j]; ++j)
            {
                int k_len = strlen(keywords[j]);
                int kw2 = keywords[j][k_len - 1] == '|';
                if (kw2)
					--k_len;

                if (!strncmp(&row -> render[i], keywords[j], k_len) && isSeparator(row -> render[i + k_len]))
                {
                    memset(&row -> hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, k_len);
                    i += k_len;
                    break;
                }
            }

            if (keywords[j] != NULL)
            {
                prev_sep = 0;
                continue;
            }
        }

        prev_sep = isSeparator(c);
        ++i;
    }

    int changed = (row -> hl_open_comment != in_comment);
    row -> hl_open_comment = in_comment;
    if (changed && row -> idx + 1 < E.num_rows)
		editorUpdateSyntax(&E.row[row -> idx + 1]);
}

int editorSyntaxToColor(int hl)
{
    switch (hl)
    {
        case HL_COMMENT:
        case HL_MLCOMMENT: return 36;
        case HL_KEYWORD1: return 33;
        case HL_KEYWORD2: return 32;
        case HL_STRING: return 35;
        case HL_NUMBER: return 31;
        case HL_MATCH: return 34;
        default: return 37;
    }
}

void editorSelectSyntaxHighlight()
{
    E.syntax = NULL;
    if (E.file_name == NULL)
		return;

    char * ext = strrchr(E.file_name, '.');
    
    for (unsigned int j = 0; j < HLDB_ENTRIES; ++j)
    {
        struct editorSyntax * s = &HLDB[j];
        unsigned int i = 0;
        while (s -> file_match[i])
        {
            int is_ext = (s -> file_match[i][0] == '.');
            if ((is_ext && ext && !strcmp(ext, s -> file_match[i])) || (!is_ext && strstr(E.file_name, s -> file_match[i])))
            {
                E.syntax = s;

                int file_row;
                for (file_row = 0; file_row < E.num_rows; ++file_row)
					editorUpdateSyntax(&E.row[file_row]);
                
				return;
            }
            ++i;
        }
    }
}

/* row operations */

int editorRowCxToRx(e_row * row, int cx)
{
    int rx = 0;
    int j;
    for (j = 0; j < cx; j++)
    {
        if (row -> chars[j] == '\t')
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        ++rx;
    }
    return rx;
}

int editorRowRxToCx(e_row * row, int rx)
{
    int cur_rx = 0;
    int cx;
    for (cx = 0; cx < row -> size; ++cx)
    {
        if (row -> chars[cx] == '\t')
			cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
        ++cur_rx;
        if (cur_rx > rx)
			return cx;
    }
    return cx;
}

void editorUpdateRow(e_row * row)
{
    int tabs = 0;
    int j;
    for (j = 0; j < row -> size; ++j)
        if (row -> chars[j] == '\t')
			++tabs;
    
    free(row -> render);
    row -> render = malloc(row -> size + tabs * (KILO_TAB_STOP - 1) + 1);

    int idx = 0;
    for (j = 0; j < row -> size; ++j)
    {
        if (row -> chars[j] == '\t')
        {
            row -> render[idx++] = ' ';
            while (idx % KILO_TAB_STOP != 0)
				row -> render[idx++] = ' ';
        }
        else
        {
            row -> render[idx++] = row -> chars[j];
        }
    }

    row -> render[idx] = '\0';
    row -> r_size = idx;

    editorUpdateSyntax(row);
}

void editorInsertRow(int at, char * s, size_t len)
{
    if (at < 0 || at > E.num_rows)
		return;

    E.row = realloc(E.row, sizeof(e_row) * (E.num_rows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(e_row) * (E.num_rows - at));
    for (int j = at + 1; j <= E.num_rows; ++j)
		++E.row[j].idx;

    E.row[at].idx = at;

    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].r_size = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hl_open_comment = 0;
    editorUpdateRow(&E.row[at]);

    ++E.num_rows;
    ++E.dirty;
}

void editorFreeRow(e_row * row)
{
    free(row -> render);
    free(row -> chars);
    free(row -> hl);
}

void editorDelRow(int at)
{
    if (at < 0 || at >= E.num_rows)
		return;
    
	editorFreeRow(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(e_row) * (E.num_rows - at - 1));
    for (int j = at; j < E.num_rows - 1; ++j)
		--E.row[j].idx;
    --E.num_rows;
    ++E.dirty;
}

void editorRowInsertChar(e_row * row, int at, int c)
{
    if (at < 0 || at > row -> size)
		at = row -> size;
    row -> chars = realloc(row -> chars, row -> size + 2);
    memmove(&row -> chars[at + 1], &row -> chars[at], row -> size - at + 1);
    row -> size++;
    row -> chars[at] = c;
    editorUpdateRow(row);
    ++E.dirty;
}

void editorRowAppendString(e_row * row, char * s, size_t len)
{
    row -> chars = realloc(row -> chars, row -> size + len + 1);
    memcpy(&row -> chars[row -> size], s, len);
    row -> size += len;
    row -> chars[row -> size] = '\0';
    editorUpdateRow(row);
    ++E.dirty;
}

void editorRowDelChar(e_row * row, int at)
{
    if (at < 0 || at >= row -> size)
		return;
    
	memmove(&row -> chars[at], &row -> chars[at + 1], row -> size - at);
    row -> size--;
    editorUpdateRow(row);
    ++E.dirty;
}

/* editor operations */

void editorInsertChar(int c)
{
    if (E.cy == E.num_rows)
		editorInsertRow(E.num_rows, "", 0);
    editorRowInsertChar(&E.row[E.cy], E.cx, c);
    ++E.cx;
}

void editorInsertNewLine()
{
    if (E.cx == 0)
		editorInsertRow(E.cy, "", 0);
    else
    {
        e_row * row = &E.row[E.cy];
        editorInsertRow(E.cy + 1, &row -> chars[E.cx], row -> size - E.cx);
        row = &E.row[E.cy];
        row -> size = E.cx;
        row -> chars[row -> size] = '\0';
        editorUpdateRow(row);
    }
    ++E.cy;
    E.cx = 0;
}

void editorDelChar()
{
    if (E.cy == E.num_rows || (E.cx == 0 && E.cy == 0))
		return;

    e_row * row = &E.row[E.cy];
    if (E.cx > 0)
    {
        editorRowDelChar(row, E.cx - 1);
        --E.cx;
    }
    else
    {
        E.cx = E.row[E.cy - 1].size;
        editorRowAppendString(&E.row[E.cy - 1], row -> chars, row -> size);
        editorDelRow(E.cy);
        --E.cy;
    }
}

/* file i/o */

char * editorRowsToString(int * buf_len)
{
    int tot_len = 0;
    int j;
    for (j = 0; j < E.num_rows; ++j)
		tot_len += E.row[j].size + 1;
    *buf_len = tot_len;

    char * buf = malloc(tot_len);
    char * p = buf;
    for (j = 0; j < E.num_rows; ++j)
    {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        ++p;
    }

    return buf;
}

void editorOpen(char * file_name)
{
    free(E.file_name);
    E.file_name = strdup(file_name);

    editorSelectSyntaxHighlight();

    FILE * fp = fopen(file_name, "r");
    if (!fp)
		die("fopen");

    char * line = NULL;
    size_t line_cap = 0;
    ssize_t line_len;

    while((line_len = getline(&line, &line_cap, fp)) != -1)
    {
        while (line_len > 0 && (line[line_len - 1] == '\n' || line[line_len - 1] == '\r'))
			--line_len;
        editorInsertRow(E.num_rows, line, line_len);
    }
    
    free(line);
    fclose(fp);
    E.dirty = 0;
}

void editorSave()
{
    if (E.file_name == NULL)
    {
        E.file_name = editorPrompt("Save as: %s (ESC to cancel)", NULL);
        if (E.file_name == NULL)
        {
            editorSetStatusMessage("Save aborted");
            return;
        }
        editorSelectSyntaxHighlight();
    }
    
    int len;
    char * buf = editorRowsToString(&len);

    int fd = open(E.file_name, O_RDWR | O_CREAT, 0644);
    if (fd != -1)
    {
        if (ftruncate(fd, len) != -1)
        {
            if (write(fd, buf, len) == len)
            {
                close(fd);
                free(buf);
                E.dirty = 0;
                editorSetStatusMessage("%d bytes written to disk", len);
                return;
            }
        }
        close(fd);
    }
    free(buf);
    editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/* find */

void editorFindCallback(char * query, int key)
{
    static int last_match = -1;
    static int direction = 1;

    static int hl_line;
    static char * hl = NULL;

    if (hl)
    {
        memcpy(E.row[hl_line].hl, hl, E.row[hl_line].r_size);
        free(hl);
        hl = NULL;
    }

    if (key == '\r' || key == '\x1b')
    {
        last_match = -1;
        direction = 1;
        return;
    }
    else if (key == ARROW_RIGHT || key == ARROW_DOWN)
		direction = 1;
    else if (key == ARROW_LEFT || key == ARROW_UP)
		direction = -1;
    else
    {
        last_match = -1;
        direction = 1;
    }

    if (last_match == -1)
		direction = 1;
    int current = last_match;

    int i;
    for (i = 0; i < E.num_rows; ++i)
    {
        current += direction;
        if (current == -1)
			current = E.num_rows - 1;
        else if (current == E.num_rows)
			current = 0;

        e_row * row = &E.row[current];
        char * match = strstr(row -> render, query);
        if (match)
        {
            last_match = current;
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row -> render);
            E.row_off = E.num_rows;

            hl_line = current;
            hl = malloc(row -> r_size);
            memcpy(hl, row -> hl, row -> r_size);
            memset(&row -> hl[match - row -> render], HL_MATCH, strlen(query));
            break;
        }
    }
}

void editorFind()
{
    int cx = E.cx;
    int cy = E.cy;
    int col_off = E.col_off;
    int row_off = E.row_off;

    char * query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)", editorFindCallback);
    if (query)
		free(query);
    else
    {
        E.cx = cx;
        E.cy = cy;
        E.col_off = col_off;
        E.row_off = row_off;
    }
}

/* append buffer */

struct abuf
{
    char * b;
    int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf * ab, const char * s, int len)
{
    char * new = realloc(ab -> b, ab -> len + len);

    if (new == NULL)
		return;
    memcpy(&new[ab -> len], s, len);
    ab -> b = new;
    ab -> len += len;
}

void abFree(struct abuf * ab)
{
    free(ab -> b);
}

/* output */

void editorScroll()
{
    E.rx = 0;
    
	if (E.cy < E.num_rows)
        E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);

    if (E.cy < E.row_off)
        E.row_off = E.cy;
    
    if (E.cy >= E.row_off + E.screen_rows)
        E.row_off = E.cy - E.screen_rows + 1;
    
    if (E.rx < E.col_off)
        E.col_off = E.rx;
    
    if (E.rx >= E.col_off + E.screen_cols)
        E.col_off = E.rx - E.screen_cols + 1;
}

void editorDrawRows(struct abuf * ab)
{
    int y;
    for (y = 0; y < E.screen_rows; ++y)
    {
        int file_row = y + E.row_off;
        if (file_row >= E.num_rows)
        {
            if (E.num_rows == 0 && y == E.screen_rows / 3)
            {
                char welcome[80];
                int welcome_len = snprintf(welcome, sizeof(welcome), "Kilo editor -- version %s", KILO_VERSION);
                if (welcome_len > E.screen_cols)
					welcome_len = E.screen_cols;
                int padding = (E.screen_cols - welcome_len) / 2;
                if (padding)
                {
                    abAppend(ab, "~", 1);
                    --padding;
                }
                while (--padding)
					abAppend(ab, " ", 1);
                abAppend(ab, welcome, welcome_len);
            }
            else
            {
                abAppend(ab, "~", 1);
            }
        }
        else
        {
            int len = E.row[file_row].r_size - E.col_off;
            if (len < 0)
				len = 0;
            if (len > E.screen_cols)
				len = E.screen_cols;
            char * c = &E.row[file_row].render[E.col_off];
            unsigned char * hl = &E.row[file_row].hl[E.col_off];
            int current_color = -1;
            int j;
            for (j = 0; j < len; ++j)
            {
                if (iscntrl(c[j]))
                {
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    abAppend(ab, "\x1b[7m", 4);
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);
                    if (current_color != -1)
                    {
                        char buf[16];
                        int c_len = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, c_len);
                    }
                }
                else if (hl[j] == HL_NORMAL)
                {
                    if (current_color != -1)
                    {
                        abAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abAppend(ab, &c[j], 1);
                }
                else
                {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != current_color)
                    {
                        current_color = color;
                        char buf[16];
                        int c_len = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        abAppend(ab, buf, c_len);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);
        }

        abAppend(ab, "\x1b[K", 3);
        abAppend(ab, "\r\n", 2);
    }
}

void editorDrawStatusBar(struct abuf * ab)
{
    abAppend(ab, "\x1b[7m", 4);
    char status[80], r_status[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s", E.file_name ? E.file_name : "[No Name]", E.num_rows, E.dirty ? "(modified)" : "");
    int r_len = snprintf(r_status, sizeof(r_status), "%s | %d/%d", E.syntax ? E.syntax -> file_type : "no ft", E.cy + 1, E.num_rows);
    if (len > E.screen_cols)
		len = E.screen_cols;
    abAppend(ab, status, len);
    while (len < E.screen_cols)
    {
        if (E.screen_cols - len == r_len)
        {
            abAppend(ab, r_status, r_len);
            break;
        }
        else
        {
            abAppend(ab, " ", 1);
            ++len;
        }
    }
    abAppend(ab, "\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf * ab)
{
    abAppend(ab, "\x1b[K", 3);
    int msg_len = strlen(E.status_msg);
    if (msg_len > E.screen_cols)
		msg_len = E.screen_cols;
    if (msg_len && time(NULL) - E.status_msg_time < 5)
		abAppend(ab, E.status_msg, msg_len);
}

void editorRefreshScreen()
{
    editorScroll();

    struct abuf ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6);
    abAppend(&ab, "\x1b[H", 3);

    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);

    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.row_off) + 1, (E.rx - E.col_off) + 1);
    abAppend(&ab, buf, strlen(buf));

    abAppend(&ab, "\x1b[?25h", 6);

    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

void editorSetStatusMessage(const char * fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.status_msg, sizeof(E.status_msg), fmt, ap);
    va_end(ap);
    E.status_msg_time = time(NULL);
}

/* input */

char * editorPrompt(char * prompt, void (*callback)(char *, int))
{
    size_t buf_size = 128;
    char * buf = malloc(buf_size);

    size_t buf_len = 0;
    buf[0] = '\0';

    while (1)
    {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE)
        {
            if (buf_len != 0)
				buf[--buf_len] = '\0';
        }
        else if (c == '\x1b')
        {
            editorSetStatusMessage("");
            if (callback)
				callback(buf, c);
            free(buf);
            return NULL;
        }
        else if (c == '\r')
        {
            if (buf_len != 0)
            {
                editorSetStatusMessage("");
                if (callback)
					callback(buf, c);
                return buf;
            }
        }
        else if (!iscntrl(c) && c < 128)
        {
            if (buf_len == buf_size - 1)
            {
                buf_size *= 2;
                buf = realloc(buf, buf_size);
            }
            buf[buf_len++] = c;
            buf[buf_len] = '\0';
        }

        if (callback)
			callback(buf, c);
    }
}

void editorMoveCursor(int key)
{
    e_row * row = (E.cy >= E.num_rows) ? NULL : &E.row[E.cy];

    switch (key)
    {
        case ARROW_LEFT:
            if (E.cx != 0)
            {
                --E.cx;
            }
            else if (E.cy > 0)
            {
                --E.cy;
                E.cx = E.row[E.cy].size;
            }
            break;
        case ARROW_RIGHT:
            if (row && E.cx < row -> size)
            {
                ++E.cx;
            }
            else if (row && E.cx == row -> size)
            {
                ++E.cy;
                E.cx = 0;
            }
            break;
        case ARROW_UP:
            if (E.cy != 0)
				--E.cy;
            break;
        case ARROW_DOWN:
            if (E.cy < E.num_rows)
				++E.cy;
            break;
    }

    row = (E.cy >= E.num_rows) ? NULL : &E.row[E.cy];
    int row_len = row ? row -> size : 0;
    if (E.cx > row_len)
		E.cx = row_len;
}

void editorProcessKeypress()
{
    static int quit_times = KILO_QUIT_TIMES;
    int c = editorReadKey();

    switch(c)
    {
        case '\r':
            editorInsertNewLine();
            break;
        
        case CTRL_KEY('q'):
            if (E.dirty && quit_times > 0)
            {
                if (quit_times > 1)
                    editorSetStatusMessage("WARNING!!! File has unsaved changes. " "Press Ctrl-Q %d more times to quit.", quit_times);
                else
                    editorSetStatusMessage("WARNING!!! File has unsaved changes. Press Ctrl-Q 1 more time to quit.");
                --quit_times;
                return;
            }
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;
        
        case CTRL_KEY('s'):
            editorSave();
            break;
        
        case HOME_KEY:
            E.cx = 0;
            break;
        
        case END_KEY:
            if (E.cy < E.num_rows)
				E.cx = E.row[E.cy].size;
            break;

        case CTRL_KEY('f'):
            editorFind();
            break;
        
        case BACKSPACE:
        case CTRL_KEY('h'):
        case DEL_KEY:
            if (c == DEL_KEY)
				editorMoveCursor(ARROW_RIGHT);
            editorDelChar();
            break;
        
        case PAGE_UP:
        case PAGE_DOWN:
            {
                if (c == PAGE_UP)
                {
                    E.cy = E.row_off;
                }
                else if (c == PAGE_DOWN)
                {
                    E.cy = E.row_off + E.screen_rows - 1;
                    if (E.cy > E.num_rows)
						E.cy = E.num_rows;
                }

                int times = E.screen_rows;
                while (--times)
                    editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            }
            break;

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;
        
        case CTRL_KEY('l'):
        case '\x1b':
            break;
        
        default:
            editorInsertChar(c);
            break;
    }

    quit_times = KILO_QUIT_TIMES;
}

/* initialization */

void initEditor()
{
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.row_off = 0;
    E.col_off = 0;
    E.num_rows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.file_name = NULL;
    E.status_msg[0] = '\0';
    E.status_msg_time = 0;
    E.syntax = NULL;

    if (getWindowSize(&E.screen_rows, &E.screen_cols) == -1)
		die("getWindowSize");
    E.screen_rows -= 2;
}

int main(int argc, char * argv[])
{
    enableRawMode();
    initEditor();
    if (argc >= 2)
        editorOpen(argv[1]);
    
    editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (1)
    {
        editorRefreshScreen();
        editorProcessKeypress();
    }
    return 0;
}