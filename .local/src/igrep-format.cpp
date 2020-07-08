#include <algorithm>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <string>
#include <string_view>

int count_chars(std::string_view text) {
    auto n = text.size();
    auto escaped = false;
    auto prev = '\0';
    for (auto c : text) {
        if (escaped) {
            --n;
            escaped = c != 'm';
        } else if (c == '[' && prev == 0x1b) {
            n -= 2;
            escaped = true;
        } else if ((c & 0xc0) == 0x80) {
            --n;
        }
        prev = c;
    }
    return n;
}

int main(int argc, char **argv) {
    auto columns = std::atoi(argv[1]) - 4;

    constexpr auto filename_prefix = sizeof("\x1b[0m\x1b[35m") - 1, filename_suffix = sizeof("\x1b[0m") - 1;
    constexpr auto line_no_prefix = sizeof("\0\x1b[0m\x1b[32m") - 1, line_no_suffix = sizeof("\x1b[0m") - 1;
    constexpr auto column_no_prefix = sizeof(":\x1b[0m") - 1, column_no_suffix = sizeof("\x1b[0m") - 1;
    constexpr auto text_prefix = sizeof(":") - 1, text_suffix = sizeof("") - 1;

    std::ios::sync_with_stdio(false);

    for (std::string line; std::getline(std::cin, line);) {
        auto i = line.find('\0', filename_prefix + 1 + filename_suffix);
        auto j = line.find(':', i + line_no_prefix + 1 + line_no_suffix);
        auto k = line.find(':', j + column_no_prefix + 1 + column_no_suffix);

        line.resize(std::remove(line.begin() + k + text_prefix, line.end() - text_suffix, 0) - line.begin());

        std::string_view filename(line.data() + filename_prefix, i - (filename_prefix + filename_suffix));
        std::string_view line_no(line.data() + i + line_no_prefix, j - i - (line_no_prefix + line_no_suffix));
        std::string_view column_no(line.data() + j + column_no_prefix, k - j - (column_no_prefix + column_no_suffix));
        std::string_view text(line.data() + k + text_prefix, line.size() - k - (text_prefix + text_suffix));

        for (auto i = k + text_prefix; i < line.size() - text_suffix; ++i)
            line[i] = line[i] == 0x1b ? 0 : line[i];
        std::cout << filename << '\0' << line_no << '\0' << column_no << '\0' << text << '\0';

        for (auto i = k + text_prefix; i < line.size() - text_suffix; ++i)
            line[i] = line[i] == '\t' ? ' ' : line[i] ? line[i] : 0x1b;
        text.remove_prefix(std::min(text.find_first_not_of(' '), text.size()));
        if (!text.empty())
            text.remove_suffix(text.size() - text.find_last_not_of(' ') - 1);

        auto padding = std::max(columns - count_chars(text) - count_chars(filename) - (int)line_no.size(), 1);
        std::cout << text << std::setw(padding) << ' ' << "\x1b[90m" << filename << ':' << line_no << "\x1b[0m\n";
    }
    return 0;
}
