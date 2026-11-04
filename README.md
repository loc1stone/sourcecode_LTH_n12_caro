# Cờ caro (Gomoku) bằng Haskell

Trò chơi cờ caro chạy trong terminal, viết cho đồ án cuối kỳ *Lập trình hàm với Haskell*.
Có ba chế độ chơi: **hai người cùng máy**, **đấu với máy** (3 mức), **hai người qua mạng** (TCP).
Ván cờ được **tự động lưu** ở luồng nền và có thể nạp lại.
Có thể chơi bằng **dòng lệnh** hoặc bằng **giao diện đồ họa trên trình duyệt** (`--gui`, phần bonus).

## 1. Cài đặt

Công cụ: **GHC** và **Cabal** (cài bằng [GHCup](https://www.haskell.org/ghcup/)).

```text
ghcup install ghc 9.4.7      # hoặc phiên bản 9.4 – 9.6 khác
ghcup install cabal          # cần cabal >= 3.8
ghcup set ghc 9.4.7
```

Phiên bản đã dùng để xây dựng và kiểm thử (Ubuntu 24.04):

| Công cụ / thư viện | Phiên bản |
|---|---|
| GHC | 9.4.7 |
| cabal-install | 3.8.1.0 |
| base / containers / stm / directory | đi kèm GHC 9.4.7 (containers 0.6.7, stm 2.5.1.0, directory 1.3.7.1) |
| parsec | 3.1.16.1 (đi kèm GHC) |
| bytestring 0.11.5.2 / text 2.0.2 / process 1.6.17.0 / template-haskell 2.19.0.0 | đi kèm GHC (dùng cho giao diện đồ họa) |
| network | 3.1.4.0 (yêu cầu `>= 3.1`) — **thư viện ngoài duy nhất**; `cabal` tự tải |

> Hãy ghi lại phiên bản máy của bạn bằng `ghc --version` và `cabal --version` nếu khác bảng trên.

Trên Windows, GHCup đã kèm MSYS2 nên `network` biên dịch bình thường.

Dùng **giao diện đồ họa** `--gui`: trang web luôn hiển thị đúng tiếng Việt, không phụ thuộc console. Chỉ các dòng nhật ký in ở terminal có thể lỗi, khi đó thêm `--ascii`.

## 2. Biên dịch, chạy, kiểm thử

```text
cabal build                      # biên dịch
cabal run caro                   # hai người chơi trên một máy, bàn 15x15, luật caro
cabal run caro -- --gui          # giao diện đồ họa: mở trình duyệt tại http://localhost:8123/
cabal run caro -- --help         # xem mọi tùy chọn
cabal test                       # chạy bộ kiểm thử (145 kiểm thử)
cabal run caro -- --bench        # đo thời gian máy tính nước đi với 1/2/4 luồng
```

Chương trình dùng `-threaded -rtsopts "-with-rtsopts=-N"`, tức là tự dùng mọi nhân CPU.
Muốn chọn số nhân: `cabal run caro -- --bot hard +RTS -N4`.

## 3. Cách chơi

Tọa độ gồm **cột (chữ)** và **hàng (số)**, ví dụ `h8`. Các cách gõ đều được chấp nhận:
`h8`, `H8`, `h 8`, `8h`, `8 8`, `8,8`, `move h8`, `play h8`.

| Lệnh | Tác dụng |
|---|---|
| `h8` | đặt quân |
| `undo` | hoàn tác (đấu với máy: lùi cả nước của máy) |
| `hint` | gợi ý một nước đi |
| `resign` | đầu hàng |
| `save [tệp]` / `load tệp` | lưu / nạp ván cờ |
| `new [cỡ]` | ván mới, có thể đổi cỡ bàn (9..26) |
| `show`, `help`, `quit` | xem bàn, hướng dẫn, thoát (tự lưu nếu đã có nước đi) |

Trên bàn cờ: `[X]` là nước vừa đi, `<X>` là dãy quân thắng.

### Luật

* **caro** (mặc định): đủ 5 quân liên tiếp thì thắng, **trừ khi dãy bị chặn cả hai đầu** (bởi quân đối thủ hoặc mép bàn).
* **freestyle**: 5 quân liên tiếp trở lên là thắng.
* Bàn đầy mà không ai thắng thì **hòa**. Có thể **hết giờ** (`--time`) hoặc **đầu hàng**.

### Ví dụ

```text
cabal run caro -- --bot normal                    # đấu máy mức vừa, bạn cầm X
cabal run caro -- --bot hard --play O --size 19   # máy đi trước, bàn 19x19
cabal run caro -- --time 30 --variant freestyle   # 30 giây/nước, luật tự do
cabal run caro -- --resume                        # nạp lại ván tự lưu gần nhất (caro.sav)
```

**Chơi qua mạng** (mở hai terminal, hoặc hai máy cùng mạng):

```text
# máy 1 (cầm X, đi trước)             # máy 2 (cầm O)
cabal run caro -- --host 5000 --ascii        cabal run caro -- --join localhost:5000 --ascii
```

Chế độ mạng không hỗ trợ `undo`, `new`, `load` và đồng hồ.

### Giao diện đồ họa (bonus)

```text
cabal run caro -- --gui                          # hai người cùng máy, trình duyệt tự mở
cabal run caro -- --gui --bot hard --time 30     # đấu máy, 30 giây/nước
cabal run caro -- --gui --host 5000  --ascii            # chủ phòng mạng, chơi bằng giao diện đồ họa
cabal run caro -- --gui --port 9000 --no-browser # đổi cổng (mặc định 8123), không tự mở trình duyệt
```

Giao diện là một trang web do **chính chương trình Haskell phục vụ** (không cần cài thêm thư viện đồ họa),
chạy được cùng `--bot`, `--host`, `--join`, `--time`, `--size`, `--variant`, `--resume`.
Nếu trình duyệt không tự mở, hãy mở địa chỉ chương trình in ra.

* **Hiển thị trạng thái:** bàn cờ (kèm nước vừa đi, dãy thắng, gợi ý), lượt chơi, đồng hồ, kết quả, nhật ký thông báo.
* **Thao tác:** nhấp vào giao điểm để đặt quân (hoặc phím mũi tên + Enter); các nút Hoàn tác, Gợi ý, Lưu, Nạp, Đầu hàng, Ván mới (chọn cỡ 9..26), Thoát.
* **Phản hồi khi không hợp lệ:** đặt vào ô đã có quân, sai lượt, ván đã kết thúc, cỡ bàn sai, `undo` khi chơi mạng... đều hiện thông báo đỏ ngay trên bàn cờ và trong nhật ký; các nút không dùng được sẽ bị làm mờ.
* Thích ứng màn hình hẹp (điện thoại) và chế độ tối.

### Ví dụ lỗi nhập

```text
> h8x
Không hiểu lệnh:
  h8x
    ^
Ký tự không mong đợi: 'x'.
Mong đợi: chữ số hoặc hết lệnh.
Gõ 'help' để xem hướng dẫn.
```

## 4. Cấu trúc mã nguồn và vị trí các chủ đề môn học

| Module | Trách nhiệm | Chủ đề môn học |
|---|---|---|
| `Caro.Types` | kiểu dữ liệu miền bài toán | **ADT**, kiểu loại trừ trạng thái sai (`Size`/`Config` chỉ tạo qua `mkSize`/`mkConfig`), lỗi có kiểu (`MoveError`, `ConfigError`), `Status`/`WinReason` |
| `Caro.Rules` | luật chơi, **hoàn toàn thuần** | hàm thuần, đệ quy, `foldM` (`replay`), `takeWhile`/`iterate` (`walk`), `Either` cho lỗi |
| `Caro.Pretty` | hiển thị cho người dùng | **type class** `Pretty` tự định nghĩa (có phương thức mặc định `prettyList`, instance cho 9 kiểu và cho `[a]`) |
| `Caro.Parser` | đọc lệnh và tệp lưu | **parser combinator** (Parsec): nhiều dạng đầu vào, lỗi tiếng Việt chỉ đúng vị trí |
| `Caro.Protocol` | gói tin mạng | parser cho giao thức văn bản |
| `Caro.AI` | máy đánh cờ (thuần) | hàm bậc cao, đệ quy (negamax), `Semigroup`/`Monoid` cho kiểu `Best` |
| `Caro.Parallel` | tính nước đi song song | `forkOn`, `MVar`, `evaluate` (đánh giá chặt), gộp bằng `Monoid` |
| `Caro.Session` | **lõi ứng dụng thuần** `step :: Event -> Session -> (Session, [Effect])` | tách phần thuần / phần hiệu ứng, trạng thái truyền tường minh |
| `Caro.App` | **ranh giới hiệu ứng** | `IO`, luồng nền, **STM** (`TQueue`, `TVar`, `check`), `MVar` |
| `Caro.Storage` | lưu / nạp tệp | tệp, ghi nguyên tử (tệp tạm + đổi tên), `readFile` lười |
| `Caro.Net` | socket TCP | **socket** (`network`) |
| `Caro.Cli` | tham số dòng lệnh | `Either` cho lỗi |
| `Caro.Ascii` | chế độ `--ascii`: bỏ dấu tiếng Việt cho console không hiện UTF-8 | hàm thuần, `Data.Map` |
| `Caro.Gui` | **giao diện đồ họa**: máy chủ HTTP nhỏ, định tuyến, trạng thái JSON | `network`/`bytestring`, hàm thuần cho HTTP/định tuyến/bảo mật, long-poll bằng STM (`check` + `registerDelay`) |
| `Caro.Json` | mã hóa JSON tối giản | ADT `J`, `ShowS` |
| `Caro.Gui.Assets` | nhúng `gui/index.html` lúc biên dịch | Template Haskell |
| `gui/index.html` | trang giao diện (HTML + CSS + JavaScript, vẽ bằng canvas) | |
| `app/Main.hs` | điểm vào, nối các phần | |
| `test/Spec.hs` | bộ kiểm thử | |

### Ranh giới giữa phần thuần và phần có hiệu ứng

```text
 người gõ ──► EvLine ┐
 đồng hồ ───► EvTick ┤                       ┌─► Say / ShowBoard   (in ra)
 máy xong ──► EvBotMove ├─► TQueue ─► step ─►├─► Send              (gửi gói tin)
 đối thủ ───► EvNet  ┘     (STM)    (THUẦN)  ├─► SaveTo / LoadFrom (tệp)
                                             └─► StartBot / Stop
```

* `Caro.Rules`, `Caro.AI`, `Caro.Session`, `Caro.Parser` **không có IO** và được kiểm thử trực tiếp.
* `Caro.App` là nơi duy nhất thực thi `Effect` bằng IO. Cách hiển thị tách khỏi lõi qua bản ghi `Output` (dòng lệnh: in văn bản; giao diện đồ họa: ghi nhật ký cho trình duyệt), nên cùng một lõi phục vụ cả hai giao diện. Lỗi (đọc tệp, gửi gói tin, tính toán) được bắt và chuyển thành thông báo; chương trình không dừng đột ngột.

### Monad và xử lý lỗi

* `Either MoveError` (`applyMove`, `undoMove`, `replay`), `Either ConfigError` (`mkConfig`), `Either ParseError` (parser), `Either String` (`loadGame`).
* `Maybe`: `bestMove`, `winningLine`, `bestPos`.
* `IO` + STM ở `Caro.App`. Dự án dùng `Either` và trạng thái truyền tường minh trong `step` thay cho `State` (cho dễ kiểm thử); nếu cần `State`, `step` chuyển đổi trực tiếp được.

## 5. Đánh giá lười và đánh giá chặt (cho báo cáo)

1. **Lười có lợi** — `Caro.Rules.walk`: `iterate (addPos d) from` là danh sách *vô hạn*; `takeWhile` chỉ duyệt đến khi gặp ô trống/quân đối thủ/ngoài bàn. Kiểm thử `walk: danh sách vô hạn nhưng dừng nhờ đánh giá lười` chứng minh nó dừng.
2. **Chặt là bắt buộc để song song** — `Caro.Parallel`: nếu luồng con chỉ `putMVar box (foldMap ...)` thì `MVar` chứa một *thunk chưa tính*, toàn bộ việc nặng bị dồn về luồng gọi và mất song song. `evaluate` ép tính xong trong luồng con (kiểu `Best` có trường chặt `!Int !Pos`).
3. **Đọc tệp lười** — `Caro.Storage.loadGame`: `readFile` đọc lười và giữ tệp mở; `length t `seq`` ép đọc hết để tệp đóng trước khi luồng tự lưu ghi đè.
4. Trường chặt (`!`) ở `Pos`, `Config`, `GameState`... tránh tích tụ thunk.

## 6. Lập trình song song và đồng bộ hóa

**Các tác vụ chạy đồng thời** (xem đầu `Caro/App.hs`):

| Tác vụ | Vai trò |
|---|---|
| luồng nhập (`inputThread`) | đọc dòng người gõ |
| đồng hồ (`clockThread`) | mỗi giây một nhịp, tính giờ từng nước |
| tự động lưu (`autosaveThread`) | chờ ván thay đổi rồi ghi tệp |
| tác vụ máy (`botTask`) | tính nước đi bằng nhiều luồng con (`parBestMove`) |
| đọc mạng (`netReaderThread`) | nhận gói tin từ đối thủ |
| máy chủ HTTP của GUI (`serveGui`) | mỗi kết nối trình duyệt một luồng; thao tác trở thành sự kiện đẩy vào `TQueue` |
| luồng chính (`runLoop`) | xử lý tuần tự mọi sự kiện |

**Cơ chế đồng bộ hóa và các tình huống tranh chấp:**

| Nguy cơ | Cách ngăn |
|---|---|
| Nhiều luồng cùng sửa ván cờ | Mọi luồng chỉ **ghi sự kiện vào `TQueue`**; chỉ luồng chính đọc hàng đợi và ghi `TVar Session` (một giao dịch `atomically`). |
| Tự động lưu thấy trạng thái nửa vời | Luồng tự lưu đọc **ảnh chụp** `(version, game, path)` trong **một giao dịch STM**. |
| Luồng tự lưu thăm dò liên tục | Dùng `check (v /= seen)`: STM tự `retry` đến khi `TVar` đổi, không tốn CPU. |
| Hai nơi cùng ghi tệp lưu (tự lưu và `save`/`quit`) | `MVar ()` làm khóa; ghi ra tệp tạm rồi `renameFile` nên tệp cũ không bao giờ bị ghi dở. |
| **Kết quả "cũ" của máy**: người `undo`/`new`/`load` trong lúc máy đang nghĩ | Mỗi lần giao việc cho máy kèm **số phiên bản**; `onBotMove` bỏ kết quả nếu `sVersion` đã đổi (có kiểm thử). |
| Các dòng in xen lẫn | Chỉ luồng chính được in ra (`envOut`). |
| Thứ tự xong việc của các luồng con tính toán | Gộp bằng `Monoid` `Best` có tính kết hợp + giao hoán, hòa điểm thì ô nhỏ hơn thắng ⇒ kết quả như bản tuần tự (có kiểm thử so sánh với 1, 2, 3, 8 luồng). |
| Trình duyệt chờ trạng thái mới | **Long-poll bằng STM**: yêu cầu `/api/state` dừng ở `check (stamp /= stampTrìnhDuyệt || hếtGiờ)`; `registerDelay` đặt hạn chót 20 giây. Trạng thái đổi là được đánh thức ngay, không thăm dò. |
| Trang web lạ gửi lệnh tới cổng GUI | Chỉ lắng nghe `127.0.0.1`; kiểm tra `Host` (chống DNS rebinding) và `Origin`; chỉ có các thao tác cố định (không truyền được tên tệp tùy ý cho `save`/`load`); có tiêu đề CSP. Các quy tắc này là hàm thuần có kiểm thử. |
| Chạy trên nhiều capability | `forkOn (i mod caps)` ghim từng luồng; build bằng `-threaded -with-rtsopts=-N`. |

**Minh chứng song song:** `cabal run caro -- --bench` in số capability và thời gian tính cùng một nước đi
(độ sâu 5) với 1, 2, 4 luồng. Chạy trên máy nhiều nhân và chụp kết quả cho báo cáo; thêm `+RTS -s` để xem thống kê của runtime.
Mức tăng tốc phụ thuộc số nhân CPU — trên máy một nhân sẽ không nhanh hơn.

## 7. Lưu tệp và mạng

* **Tệp lưu** (văn bản): cấu hình + danh sách nước đi. Khi nạp, các nước được **chơi lại** qua `replay`, nên tệp sai luật bị từ chối thay vì tạo ra trạng thái sai.

  ```text
  caro-save 1
  size: 15
  variant: caro
  time: unlimited
  moves: h8 i9 h9
  ```
* **Mạng**: TCP, mỗi dòng một gói tin: `HELLO 15 caro` (chủ phòng báo cỡ bàn và luật), `MOVE hàng cột` (từ 0), `RESIGN`, `BYE`. Hai bên cùng chạy hàm thuần `applyMove` nên trạng thái luôn khớp nhau.
* Cách kiểm thử mạng: `--host` và `--join localhost:PORT` ở hai terminal; trong `Spec.hs`, phần giao thức và `step` ở chế độ `Online` được kiểm thử thuần.

## 8. Kiểm thử

```text
cabal test
```

Gồm 145 kiểm thử:

* chế độ `--ascii`: bỏ dấu đúng cho mọi chữ cái tiếng Việt, mọi thông báo của chương trình đều chuyển được thành ASCII thuần;
* luật: bốn hướng thắng, 4 quân chưa thắng, dãy bị chặn (caro) và không bị chặn (freestyle), biên bàn, 6 quân, hòa, hết giờ, đầu hàng, undo;
* **property** trên 60 ván ngẫu nhiên: phát hiện thắng tăng dần == quét toàn bàn (cài đặt độc lập), số quân == số nước, undo hết về ván mới, lưu rồi nạp cho lại đúng ván;
* parser: 26 dạng đúng, 14 dạng sai, nội dung thông báo lỗi;
* giao thức mạng, tham số dòng lệnh;
* máy: thắng ngay khi có thể, chặn bốn quân, song song == tuần tự;
* `step` thuần: mọi nhánh của phiên chơi, kể cả loại bỏ kết quả máy đã cũ;
* đồng thời thật: ván trọn vẹn qua `runLoop`, 16 luồng cùng đẩy nước đi, luồng tự lưu, đồng hồ + máy chạy cùng lúc;
* giao diện đồ họa: JSON, đọc/định tuyến yêu cầu HTTP, chặn `Host`/`Origin` lạ, trạng thái JSON; và **máy chủ HTTP thật** trên cổng tự chọn (đặt quân, nước sai, long-poll, undo/hint/new, từ chối yêu cầu lạ).

Ghi chú về kiểm thử hòa: ô `(r, c)` được tô màu theo tính chẵn lẻ của `⌊r/2⌋ + c`. Theo hàng màu đổi liên tục;
theo cột đổi sau mỗi 2 ô; theo cả hai đường chéo, độ dài dãy cùng màu tối đa 2. Vậy mẫu này lấp đầy bàn mà không có 5 quân liên tiếp.

## 9. Giới hạn hiện tại

* Giao diện đồ họa chạy trong **trình duyệt** (không phải cửa sổ ứng dụng riêng), chỉ nhận kết nối từ chính máy đang chạy chương trình, không có đăng nhập; nên dùng một trình duyệt cho mỗi ván.
* Ở GUI, một số lệnh chỉ có ở dòng lệnh: `save <tệp>`/`load <tệp>` với tên tệp tùy ý (GUI chỉ lưu/nạp tệp đã cấu hình bằng `--save`), gõ tọa độ.
* Tệp lưu chỉ ghi các nước đi, nên kết quả do **hết giờ** hoặc **đầu hàng** không được lưu lại.
* Mạng: một ván, không mã hóa, không kết nối lại; không dùng đồng hồ, `undo`, `new`, `load`.
* Máy dùng heuristic + nhìn trước vài nước, mạnh vừa phải (không có tìm kiếm đe dọa chuyên sâu). Máy luôn cho cùng một nước với cùng một thế cờ.
* Cỡ bàn 9..26 (tên cột là chữ cái a..z).

## 10. Phân công và đóng góp 

| Thành viên | Phần phụ trách (module / chức năng) | Tự đánh giá |
|Nguyễn Hữu lộc|Module-chức năng -luật-giao diện|100%|
