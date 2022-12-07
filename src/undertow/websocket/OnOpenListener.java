package undertow.websocket;

import io.undertow.websockets.core.WebSocketChannel;

public interface OnOpenListener {
  void onOpen(WebSocketChannel channel, Object context);
}
