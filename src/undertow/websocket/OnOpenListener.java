package undertow.websocket;

import io.undertow.websockets.core.WebSocketChannel;

/**
 * Interface to add `onOpen` handler in websocket channel listener.
 */
public interface OnOpenListener {
  void onOpen(WebSocketChannel channel);
}
