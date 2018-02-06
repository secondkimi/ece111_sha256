module sha256(input logic clk, reset_n, start,
 input logic [31:0] message_addr, size, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);
 	
 enum logic [3:0] {IDLE=4'b0000, PRO_READ=4'b0001, SET_ONLY=4'b0010, READ_AND_SET=4'b0011, READ_ONLY=4'b1000, DONE=4'b0101, PADDING=4'B0110, PROT=4'b0111} state;

  logic [15:0] rc, wc; // read and write counters
  
  logic [31:0] blk_counter; // varies from 0 to (# of blocks - 1)
  logic[31:0] num_blks;// assigned by the function call 
  logic [31:0] S0, s1, maj, t1, t2, ch;
  logic [7:0] t; // t varies from 0 to 63

  assign mem_clk = clk;
  parameter int sha256_k[0:63] = '{
 32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
 32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
 32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
 32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
 32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
 32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
 32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
 32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
};

logic [31:0] H0 = 32'h6a09e667;
logic [31:0] H1 = 32'hbb67ae85;
logic [31:0] H2 = 32'h3c6ef372;
logic [31:0] H3 = 32'ha54ff53a;
logic [31:0] H4 = 32'h510e527f;
logic [31:0] H5 = 32'h9b05688c;
logic [31:0] H6 = 32'h1f83d9ab;
logic [31:0] H7 = 32'h5be0cd19;

logic [31:0] A; 
logic [31:0] B; 
logic [31:0] C; 
logic [31:0] D; 
logic [31:0] E; 
logic [31:0] F; 
logic [31:0] G; 
logic [31:0] H;

logic [31:0] M[0:15];
logic is_padding_block;
logic[31:0] pad_start;
int i;

function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + sha256_k[t] + w;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction

function logic [15:0] determine_num_blocks(input logic [31:0] size);
    if ((size << 3) % 512 <= 447)
      determine_num_blocks = ((size << 3)/512) + 1;
    else
      determine_num_blocks = ((size << 3)/512) + 2;
endfunction


function logic [31:0] rrot(input logic[31:0] oldW, input logic[7:0] r);
begin	
	rrot = ((oldW << (32-r)) | (oldW>>r));
end
endfunction
  
  always_ff @(posedge clk, negedge reset_n)
  begin
    if (!reset_n) begin
      state <= IDLE;
		done <= 0;
    end else
      case (state)
      IDLE: // start
          if (start) begin 
			   // global setup goes here. Not per Block variables
				blk_counter <= 0; // increment blk_counter at the end of each M. Compare Num blks with blk_counter
				state <= PRO_READ;
				is_padding_block <= 0;
				num_blks <= determine_num_blocks(size);
          end
		PRO_READ:
			// init A to H. BLK_COUNTER should already been pointing to the right addr
			begin
				t <= 0;
				for (i=0; i<16; i=i+1) begin
					M[i] = 32'h00000000;
				end
				if(num_blks == blk_counter+1 && is_padding_block) begin
					// TODO: append in bits rather than bytes. check testbench
					M[14] = size>>29;
					M[15] = size * 8;
					state <= PROT;
				end else begin
				
					rc <= 1;
					wc <= 0;
					mem_we <= 0;
					mem_addr <= message_addr+blk_counter*16;
					state <= READ_ONLY;
				end
			end
		READ_ONLY:
			if(blk_counter*64 + 4*rc >= size) begin
				state <= READ_AND_SET;
			end
			else begin
				mem_we <= 0;
				mem_addr <= message_addr+blk_counter*16 + rc;
				rc <= rc + 1;
				state <= READ_AND_SET;
			end
		READ_AND_SET:
			begin
			M[wc] <= mem_read_data;
			if(blk_counter*64 + rc*4 >= size && (size % 64) != 0) begin	
				state <= PADDING;
			end
			else begin
				wc <= wc + 1;
				if(rc < 16) begin		
					mem_we <= 0;
					mem_addr <= message_addr+blk_counter*16 + rc;
					rc <= rc + 1;
					state <= READ_AND_SET;
				end else begin
					state <= SET_ONLY;
				end				
			end
		SET_ONLY:
			begin
				M[wc] <= mem_read_data;
				state <= PROT;
			end
		PADDING:
		begin
			// TODO: check testbench
			case ((size - blk_counter*64) % 4)
				0: M[(size-blk_counter*64)/4] = 32'h80000000;
				1: M[(size-blk_counter*64)/4] = M[(size-blk_counter*64)/4] & 32'h FF000000 | 32'h 00800000;
				2: M[(size-blk_counter*64)/4] = M[(size-blk_counter*64)/4] & 32'h FFFF0000 | 32'h 00008000;
				3: M[(size-blk_counter*64)/4] = M[(size-blk_counter*64)/4] & 32'h FFFFFF00 | 32'h 00000080;
			endcase
			
			if (blk_counter < num_blks - 1) begin
				// only need to add one extra bit to the tail and fill all with zero
				is_padding_block <= 1'b1;
				
			end else begin
				// add one extra bit to the tail and fill the last two words with size
				M[14] = size >> 29;
				M[15] = size * 8;
			end
			state <= PROT;
		end
		PROT:
		begin
				A <= H0;
				B <= H1;
				C <= H2;
				D <= H3;
				E <= H4;
				F <= H5;
				G <= H6;
				H <= H7;
				// PROT performs 64 rounds 
		
			
		end

      endcase
  end

endmodule


/*
module calc_num_blocks(input logic [31:0] size,
                      output logic [15:0] num_blocks);

  function logic [15:0] determine_num_blocks(input logic [31:0] size);
    if ((size << 3) % 512 <= 447)
      determine_num_blocks = ((size << 3)/512) + 1;
    else
      determine_num_blocks = ((size << 3)/512) + 2;
  endfunction

  assign num_blocks = determine_num_blocks(size);
endmodule
*/