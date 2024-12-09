with open("2024 Challenge/data/input_day9.txt", "r") as file:
    data = file.read()


def calcChecksum(start_block_id, num_blocks, file_old_pos):
    # a = start_block_id                  : Lowest Block ID
    # b = start_block_id + num_blocks - 1 : Highest Block ID
    # (b + a)(b - a + 1)/2                : Sum of block IDs
    # file_old_pos/2                      : File ID
    # Sum (Block IDs * File ID) = (Sum Block IDs) * File ID
    # which simplifies to:
    return (2 * start_block_id + num_blocks - 1) * num_blocks * file_old_pos // 4


# Part 1

front_pos = 0
back_pos = len(data) - 1

passed_blocks = 0
front_blocks = int(data[front_pos])
back_blocks = int(data[back_pos])

checksum = 0

while front_pos < back_pos:
    if front_pos % 2 == 0:
        checksum += calcChecksum(passed_blocks, front_blocks, front_pos)
        passed_blocks += front_blocks

        front_pos += 1
        front_blocks = int(data[front_pos])
    else:
        if back_blocks >= front_blocks:
            checksum += calcChecksum(passed_blocks, front_blocks, back_pos)

            passed_blocks += front_blocks
            back_blocks -= front_blocks

            front_pos += 1
            front_blocks = int(data[front_pos])
        else:
            checksum += calcChecksum(passed_blocks, back_blocks, back_pos)

            passed_blocks += back_blocks
            front_blocks -= back_blocks

            back_pos -= 2
            back_blocks = int(data[back_pos])

if back_pos == front_pos:
    remaining_blocks = min(front_blocks, back_blocks)
    checksum += calcChecksum(passed_blocks, remaining_blocks, front_pos)

print(checksum)
