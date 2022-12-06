package undertow;

import clojure.lang.IFn;
import clojure.lang.ILookup;
import clojure.lang.Keyword;
import clojure.lang.RT;
import io.undertow.websockets.core.*;
import org.xnio.Pooled;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Arrays;

// TODO: Document purpose of the class

public class WebSocketChannelListener extends AbstractReceiveListener {
  private final IFn onMessage;
  private final IFn onClose;
  private final IFn onError;

  private static final Keyword k_channel = RT.keyword(null, "channel");
  private static final Keyword k_message = RT.keyword(null, "message");

  public WebSocketChannelListener(ILookup handlers) {
    this.onMessage = (IFn) handlers.valAt(RT.keyword(null, "on-message"));
    this.onClose = (IFn) handlers.valAt(RT.keyword(null, "on-close"));
    this.onError = (IFn) handlers.valAt(RT.keyword(null, "on-error"));
  }

  @Override
  protected void onError(WebSocketChannel channel,
                         Throwable error) {
    if (this.onError == null)
      super.onError(channel, error);
    else
      this.onError.invoke(RT.map(k_channel, channel,
                                 RT.keyword(null, "error"), error));
  }

  @Override
  protected void onFullTextMessage(WebSocketChannel channel,
                                   BufferedTextMessage message) throws IOException {
    if (this.onMessage == null)
      super.onFullTextMessage(channel, message);
    else
      onMessage.invoke(RT.map(k_channel, channel,
                              k_message, message.getData()));
  }

  @Override
  protected void onFullBinaryMessage(WebSocketChannel channel,
                                     BufferedBinaryMessage message) throws IOException {
    if (this.onMessage == null)
      super.onFullBinaryMessage(channel, message);
    else {
      @SuppressWarnings("deprecation")
      Pooled<ByteBuffer[]> data = message.getData();
      byte[] buffer = WebSockets.mergeBuffers(data.getResource()).array();
      byte[] bytes = Arrays.copyOf(buffer, buffer.length);
      data.free();
      onMessage.invoke(RT.map(k_channel, channel,
                              k_message, bytes));
    }
  }

  @Override
  protected void onCloseMessage(CloseMessage cm,
                                WebSocketChannel channel) {
    if (this.onError == null)
      super.onCloseMessage(cm, channel);
    else
      onClose.invoke(RT.map(k_channel, channel,
                            RT.keyword(null, "code"), cm.getCode(),
                            RT.keyword(null, "reason"), cm.getReason()));
  }
}
