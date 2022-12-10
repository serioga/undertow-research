package undertow.websockets;

import io.undertow.websockets.core.WebSocketChannel;

public interface OnOpenListener {
  void onOpen(WebSocketChannel channel);
}
